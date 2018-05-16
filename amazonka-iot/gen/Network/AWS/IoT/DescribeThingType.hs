{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeThingType
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified thing type.
--
--
module Network.AWS.IoT.DescribeThingType
    (
    -- * Creating a Request
      describeThingType
    , DescribeThingType
    -- * Request Lenses
    , dThingTypeName

    -- * Destructuring the Response
    , describeThingTypeResponse
    , DescribeThingTypeResponse
    -- * Response Lenses
    , desrsThingTypeProperties
    , desrsThingTypeName
    , desrsThingTypeId
    , desrsThingTypeMetadata
    , desrsThingTypeARN
    , desrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
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
  { _dThingTypeName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dThingTypeName' - The name of the thing type.
describeThingType
    :: Text -- ^ 'dThingTypeName'
    -> DescribeThingType
describeThingType pThingTypeName_ =
  DescribeThingType' {_dThingTypeName = pThingTypeName_}


-- | The name of the thing type.
dThingTypeName :: Lens' DescribeThingType Text
dThingTypeName = lens _dThingTypeName (\ s a -> s{_dThingTypeName = a})

instance AWSRequest DescribeThingType where
        type Rs DescribeThingType = DescribeThingTypeResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeThingTypeResponse' <$>
                   (x .?> "thingTypeProperties") <*>
                     (x .?> "thingTypeName")
                     <*> (x .?> "thingTypeId")
                     <*> (x .?> "thingTypeMetadata")
                     <*> (x .?> "thingTypeArn")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeThingType where

instance NFData DescribeThingType where

instance ToHeaders DescribeThingType where
        toHeaders = const mempty

instance ToPath DescribeThingType where
        toPath DescribeThingType'{..}
          = mconcat ["/thing-types/", toBS _dThingTypeName]

instance ToQuery DescribeThingType where
        toQuery = const mempty

-- | The output for the DescribeThingType operation.
--
--
--
-- /See:/ 'describeThingTypeResponse' smart constructor.
data DescribeThingTypeResponse = DescribeThingTypeResponse'
  { _desrsThingTypeProperties :: !(Maybe ThingTypeProperties)
  , _desrsThingTypeName       :: !(Maybe Text)
  , _desrsThingTypeId         :: !(Maybe Text)
  , _desrsThingTypeMetadata   :: !(Maybe ThingTypeMetadata)
  , _desrsThingTypeARN        :: !(Maybe Text)
  , _desrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'desrsThingTypeProperties' - The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
--
-- * 'desrsThingTypeName' - The name of the thing type.
--
-- * 'desrsThingTypeId' - The thing type ID.
--
-- * 'desrsThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- * 'desrsThingTypeARN' - The thing type ARN.
--
-- * 'desrsResponseStatus' - -- | The response status code.
describeThingTypeResponse
    :: Int -- ^ 'desrsResponseStatus'
    -> DescribeThingTypeResponse
describeThingTypeResponse pResponseStatus_ =
  DescribeThingTypeResponse'
    { _desrsThingTypeProperties = Nothing
    , _desrsThingTypeName = Nothing
    , _desrsThingTypeId = Nothing
    , _desrsThingTypeMetadata = Nothing
    , _desrsThingTypeARN = Nothing
    , _desrsResponseStatus = pResponseStatus_
    }


-- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
desrsThingTypeProperties :: Lens' DescribeThingTypeResponse (Maybe ThingTypeProperties)
desrsThingTypeProperties = lens _desrsThingTypeProperties (\ s a -> s{_desrsThingTypeProperties = a})

-- | The name of the thing type.
desrsThingTypeName :: Lens' DescribeThingTypeResponse (Maybe Text)
desrsThingTypeName = lens _desrsThingTypeName (\ s a -> s{_desrsThingTypeName = a})

-- | The thing type ID.
desrsThingTypeId :: Lens' DescribeThingTypeResponse (Maybe Text)
desrsThingTypeId = lens _desrsThingTypeId (\ s a -> s{_desrsThingTypeId = a})

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
desrsThingTypeMetadata :: Lens' DescribeThingTypeResponse (Maybe ThingTypeMetadata)
desrsThingTypeMetadata = lens _desrsThingTypeMetadata (\ s a -> s{_desrsThingTypeMetadata = a})

-- | The thing type ARN.
desrsThingTypeARN :: Lens' DescribeThingTypeResponse (Maybe Text)
desrsThingTypeARN = lens _desrsThingTypeARN (\ s a -> s{_desrsThingTypeARN = a})

-- | -- | The response status code.
desrsResponseStatus :: Lens' DescribeThingTypeResponse Int
desrsResponseStatus = lens _desrsResponseStatus (\ s a -> s{_desrsResponseStatus = a})

instance NFData DescribeThingTypeResponse where
