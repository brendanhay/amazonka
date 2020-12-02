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
-- Module      : Network.AWS.IoT.DescribeDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides details about a dimension that is defined in your AWS account.
module Network.AWS.IoT.DescribeDimension
  ( -- * Creating a Request
    describeDimension,
    DescribeDimension,

    -- * Request Lenses
    ddName,

    -- * Destructuring the Response
    describeDimensionResponse,
    DescribeDimensionResponse,

    -- * Response Lenses
    dddrsLastModifiedDate,
    dddrsArn,
    dddrsStringValues,
    dddrsName,
    dddrsCreationDate,
    dddrsType,
    dddrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDimension' smart constructor.
newtype DescribeDimension = DescribeDimension' {_ddName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDimension' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddName' - The unique identifier for the dimension.
describeDimension ::
  -- | 'ddName'
  Text ->
  DescribeDimension
describeDimension pName_ = DescribeDimension' {_ddName = pName_}

-- | The unique identifier for the dimension.
ddName :: Lens' DescribeDimension Text
ddName = lens _ddName (\s a -> s {_ddName = a})

instance AWSRequest DescribeDimension where
  type Rs DescribeDimension = DescribeDimensionResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeDimensionResponse'
            <$> (x .?> "lastModifiedDate")
            <*> (x .?> "arn")
            <*> (x .?> "stringValues")
            <*> (x .?> "name")
            <*> (x .?> "creationDate")
            <*> (x .?> "type")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeDimension

instance NFData DescribeDimension

instance ToHeaders DescribeDimension where
  toHeaders = const mempty

instance ToPath DescribeDimension where
  toPath DescribeDimension' {..} =
    mconcat ["/dimensions/", toBS _ddName]

instance ToQuery DescribeDimension where
  toQuery = const mempty

-- | /See:/ 'describeDimensionResponse' smart constructor.
data DescribeDimensionResponse = DescribeDimensionResponse'
  { _dddrsLastModifiedDate ::
      !(Maybe POSIX),
    _dddrsArn :: !(Maybe Text),
    _dddrsStringValues ::
      !(Maybe (List1 Text)),
    _dddrsName :: !(Maybe Text),
    _dddrsCreationDate :: !(Maybe POSIX),
    _dddrsType :: !(Maybe DimensionType),
    _dddrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeDimensionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dddrsLastModifiedDate' - The date the dimension was last modified.
--
-- * 'dddrsArn' - The ARN (Amazon resource name) for the dimension.
--
-- * 'dddrsStringValues' - The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
--
-- * 'dddrsName' - The unique identifier for the dimension.
--
-- * 'dddrsCreationDate' - The date the dimension was created.
--
-- * 'dddrsType' - The type of the dimension.
--
-- * 'dddrsResponseStatus' - -- | The response status code.
describeDimensionResponse ::
  -- | 'dddrsResponseStatus'
  Int ->
  DescribeDimensionResponse
describeDimensionResponse pResponseStatus_ =
  DescribeDimensionResponse'
    { _dddrsLastModifiedDate = Nothing,
      _dddrsArn = Nothing,
      _dddrsStringValues = Nothing,
      _dddrsName = Nothing,
      _dddrsCreationDate = Nothing,
      _dddrsType = Nothing,
      _dddrsResponseStatus = pResponseStatus_
    }

-- | The date the dimension was last modified.
dddrsLastModifiedDate :: Lens' DescribeDimensionResponse (Maybe UTCTime)
dddrsLastModifiedDate = lens _dddrsLastModifiedDate (\s a -> s {_dddrsLastModifiedDate = a}) . mapping _Time

-- | The ARN (Amazon resource name) for the dimension.
dddrsArn :: Lens' DescribeDimensionResponse (Maybe Text)
dddrsArn = lens _dddrsArn (\s a -> s {_dddrsArn = a})

-- | The value or list of values used to scope the dimension. For example, for topic filters, this is the pattern used to match the MQTT topic name.
dddrsStringValues :: Lens' DescribeDimensionResponse (Maybe (NonEmpty Text))
dddrsStringValues = lens _dddrsStringValues (\s a -> s {_dddrsStringValues = a}) . mapping _List1

-- | The unique identifier for the dimension.
dddrsName :: Lens' DescribeDimensionResponse (Maybe Text)
dddrsName = lens _dddrsName (\s a -> s {_dddrsName = a})

-- | The date the dimension was created.
dddrsCreationDate :: Lens' DescribeDimensionResponse (Maybe UTCTime)
dddrsCreationDate = lens _dddrsCreationDate (\s a -> s {_dddrsCreationDate = a}) . mapping _Time

-- | The type of the dimension.
dddrsType :: Lens' DescribeDimensionResponse (Maybe DimensionType)
dddrsType = lens _dddrsType (\s a -> s {_dddrsType = a})

-- | -- | The response status code.
dddrsResponseStatus :: Lens' DescribeDimensionResponse Int
dddrsResponseStatus = lens _dddrsResponseStatus (\s a -> s {_dddrsResponseStatus = a})

instance NFData DescribeDimensionResponse
