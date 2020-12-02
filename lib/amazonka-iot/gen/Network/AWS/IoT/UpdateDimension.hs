{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for a dimension. You cannot change the type of a dimension after it is created (you can delete it and re-create it).
module Network.AWS.IoT.UpdateDimension
  ( -- * Creating a Request
    updateDimension,
    UpdateDimension,

    -- * Request Lenses
    udName,
    udStringValues,

    -- * Destructuring the Response
    updateDimensionResponse,
    UpdateDimensionResponse,

    -- * Response Lenses
    udrsLastModifiedDate,
    udrsArn,
    udrsStringValues,
    udrsName,
    udrsCreationDate,
    udrsType,
    udrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDimension' smart constructor.
data UpdateDimension = UpdateDimension'
  { _udName :: !Text,
    _udStringValues :: !(List1 Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udName' - A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
--
-- * 'udStringValues' - Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
updateDimension ::
  -- | 'udName'
  Text ->
  -- | 'udStringValues'
  NonEmpty Text ->
  UpdateDimension
updateDimension pName_ pStringValues_ =
  UpdateDimension'
    { _udName = pName_,
      _udStringValues = _List1 # pStringValues_
    }

-- | A unique identifier for the dimension. Choose something that describes the type and value to make it easy to remember what it does.
udName :: Lens' UpdateDimension Text
udName = lens _udName (\s a -> s {_udName = a})

-- | Specifies the value or list of values for the dimension. For @TOPIC_FILTER@ dimensions, this is a pattern used to match the MQTT topic (for example, "admin/#").
udStringValues :: Lens' UpdateDimension (NonEmpty Text)
udStringValues = lens _udStringValues (\s a -> s {_udStringValues = a}) . _List1

instance AWSRequest UpdateDimension where
  type Rs UpdateDimension = UpdateDimensionResponse
  request = patchJSON ioT
  response =
    receiveJSON
      ( \s h x ->
          UpdateDimensionResponse'
            <$> (x .?> "lastModifiedDate")
            <*> (x .?> "arn")
            <*> (x .?> "stringValues")
            <*> (x .?> "name")
            <*> (x .?> "creationDate")
            <*> (x .?> "type")
            <*> (pure (fromEnum s))
      )

instance Hashable UpdateDimension

instance NFData UpdateDimension

instance ToHeaders UpdateDimension where
  toHeaders = const mempty

instance ToJSON UpdateDimension where
  toJSON UpdateDimension' {..} =
    object (catMaybes [Just ("stringValues" .= _udStringValues)])

instance ToPath UpdateDimension where
  toPath UpdateDimension' {..} =
    mconcat ["/dimensions/", toBS _udName]

instance ToQuery UpdateDimension where
  toQuery = const mempty

-- | /See:/ 'updateDimensionResponse' smart constructor.
data UpdateDimensionResponse = UpdateDimensionResponse'
  { _udrsLastModifiedDate ::
      !(Maybe POSIX),
    _udrsArn :: !(Maybe Text),
    _udrsStringValues :: !(Maybe (List1 Text)),
    _udrsName :: !(Maybe Text),
    _udrsCreationDate :: !(Maybe POSIX),
    _udrsType :: !(Maybe DimensionType),
    _udrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDimensionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsLastModifiedDate' - The date and time, in milliseconds since epoch, when the dimension was most recently updated.
--
-- * 'udrsArn' - The ARN (Amazon resource name) of the created dimension.
--
-- * 'udrsStringValues' - The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- * 'udrsName' - A unique identifier for the dimension.
--
-- * 'udrsCreationDate' - The date and time, in milliseconds since epoch, when the dimension was initially created.
--
-- * 'udrsType' - The type of the dimension.
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDimensionResponse ::
  -- | 'udrsResponseStatus'
  Int ->
  UpdateDimensionResponse
updateDimensionResponse pResponseStatus_ =
  UpdateDimensionResponse'
    { _udrsLastModifiedDate = Nothing,
      _udrsArn = Nothing,
      _udrsStringValues = Nothing,
      _udrsName = Nothing,
      _udrsCreationDate = Nothing,
      _udrsType = Nothing,
      _udrsResponseStatus = pResponseStatus_
    }

-- | The date and time, in milliseconds since epoch, when the dimension was most recently updated.
udrsLastModifiedDate :: Lens' UpdateDimensionResponse (Maybe UTCTime)
udrsLastModifiedDate = lens _udrsLastModifiedDate (\s a -> s {_udrsLastModifiedDate = a}) . mapping _Time

-- | The ARN (Amazon resource name) of the created dimension.
udrsArn :: Lens' UpdateDimensionResponse (Maybe Text)
udrsArn = lens _udrsArn (\s a -> s {_udrsArn = a})

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
udrsStringValues :: Lens' UpdateDimensionResponse (Maybe (NonEmpty Text))
udrsStringValues = lens _udrsStringValues (\s a -> s {_udrsStringValues = a}) . mapping _List1

-- | A unique identifier for the dimension.
udrsName :: Lens' UpdateDimensionResponse (Maybe Text)
udrsName = lens _udrsName (\s a -> s {_udrsName = a})

-- | The date and time, in milliseconds since epoch, when the dimension was initially created.
udrsCreationDate :: Lens' UpdateDimensionResponse (Maybe UTCTime)
udrsCreationDate = lens _udrsCreationDate (\s a -> s {_udrsCreationDate = a}) . mapping _Time

-- | The type of the dimension.
udrsType :: Lens' UpdateDimensionResponse (Maybe DimensionType)
udrsType = lens _udrsType (\s a -> s {_udrsType = a})

-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDimensionResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\s a -> s {_udrsResponseStatus = a})

instance NFData UpdateDimensionResponse
