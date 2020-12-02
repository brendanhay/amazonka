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
-- Module      : Network.AWS.IoT.DescribeThingType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing type.
module Network.AWS.IoT.DescribeThingType
  ( -- * Creating a Request
    describeThingType,
    DescribeThingType,

    -- * Request Lenses
    dThingTypeName,

    -- * Destructuring the Response
    describeThingTypeResponse,
    DescribeThingTypeResponse,

    -- * Response Lenses
    dtttrsThingTypeProperties,
    dtttrsThingTypeName,
    dtttrsThingTypeId,
    dtttrsThingTypeMetadata,
    dtttrsThingTypeARN,
    dtttrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the DescribeThingType operation.
--
--
--
-- /See:/ 'describeThingType' smart constructor.
newtype DescribeThingType = DescribeThingType'
  { _dThingTypeName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dThingTypeName' - The name of the thing type.
describeThingType ::
  -- | 'dThingTypeName'
  Text ->
  DescribeThingType
describeThingType pThingTypeName_ =
  DescribeThingType' {_dThingTypeName = pThingTypeName_}

-- | The name of the thing type.
dThingTypeName :: Lens' DescribeThingType Text
dThingTypeName = lens _dThingTypeName (\s a -> s {_dThingTypeName = a})

instance AWSRequest DescribeThingType where
  type Rs DescribeThingType = DescribeThingTypeResponse
  request = get ioT
  response =
    receiveJSON
      ( \s h x ->
          DescribeThingTypeResponse'
            <$> (x .?> "thingTypeProperties")
            <*> (x .?> "thingTypeName")
            <*> (x .?> "thingTypeId")
            <*> (x .?> "thingTypeMetadata")
            <*> (x .?> "thingTypeArn")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeThingType

instance NFData DescribeThingType

instance ToHeaders DescribeThingType where
  toHeaders = const mempty

instance ToPath DescribeThingType where
  toPath DescribeThingType' {..} =
    mconcat ["/thing-types/", toBS _dThingTypeName]

instance ToQuery DescribeThingType where
  toQuery = const mempty

-- | The output for the DescribeThingType operation.
--
--
--
-- /See:/ 'describeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'
  { _dtttrsThingTypeProperties ::
      !(Maybe ThingTypeProperties),
    _dtttrsThingTypeName :: !(Maybe Text),
    _dtttrsThingTypeId :: !(Maybe Text),
    _dtttrsThingTypeMetadata ::
      !(Maybe ThingTypeMetadata),
    _dtttrsThingTypeARN :: !(Maybe Text),
    _dtttrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtttrsThingTypeProperties' - The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
--
-- * 'dtttrsThingTypeName' - The name of the thing type.
--
-- * 'dtttrsThingTypeId' - The thing type ID.
--
-- * 'dtttrsThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- * 'dtttrsThingTypeARN' - The thing type ARN.
--
-- * 'dtttrsResponseStatus' - -- | The response status code.
describeThingTypeResponse ::
  -- | 'dtttrsResponseStatus'
  Int ->
  DescribeThingTypeResponse
describeThingTypeResponse pResponseStatus_ =
  DescribeThingTypeResponse'
    { _dtttrsThingTypeProperties = Nothing,
      _dtttrsThingTypeName = Nothing,
      _dtttrsThingTypeId = Nothing,
      _dtttrsThingTypeMetadata = Nothing,
      _dtttrsThingTypeARN = Nothing,
      _dtttrsResponseStatus = pResponseStatus_
    }

-- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
dtttrsThingTypeProperties :: Lens' DescribeThingTypeResponse (Maybe ThingTypeProperties)
dtttrsThingTypeProperties = lens _dtttrsThingTypeProperties (\s a -> s {_dtttrsThingTypeProperties = a})

-- | The name of the thing type.
dtttrsThingTypeName :: Lens' DescribeThingTypeResponse (Maybe Text)
dtttrsThingTypeName = lens _dtttrsThingTypeName (\s a -> s {_dtttrsThingTypeName = a})

-- | The thing type ID.
dtttrsThingTypeId :: Lens' DescribeThingTypeResponse (Maybe Text)
dtttrsThingTypeId = lens _dtttrsThingTypeId (\s a -> s {_dtttrsThingTypeId = a})

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
dtttrsThingTypeMetadata :: Lens' DescribeThingTypeResponse (Maybe ThingTypeMetadata)
dtttrsThingTypeMetadata = lens _dtttrsThingTypeMetadata (\s a -> s {_dtttrsThingTypeMetadata = a})

-- | The thing type ARN.
dtttrsThingTypeARN :: Lens' DescribeThingTypeResponse (Maybe Text)
dtttrsThingTypeARN = lens _dtttrsThingTypeARN (\s a -> s {_dtttrsThingTypeARN = a})

-- | -- | The response status code.
dtttrsResponseStatus :: Lens' DescribeThingTypeResponse Int
dtttrsResponseStatus = lens _dtttrsResponseStatus (\s a -> s {_dtttrsResponseStatus = a})

instance NFData DescribeThingTypeResponse
