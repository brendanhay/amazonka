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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
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
    , drsThingTypeProperties
    , drsThingTypeName
    , drsThingTypeMetadata
    , drsResponseStatus
    ) where

import           Network.AWS.IoT.Types
import           Network.AWS.IoT.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for the DescribeThingType operation.
--
--
--
-- /See:/ 'describeThingType' smart constructor.
newtype DescribeThingType = DescribeThingType'
    { _dThingTypeName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeThingType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dThingTypeName' - The name of the thing type.
describeThingType
    :: Text -- ^ 'dThingTypeName'
    -> DescribeThingType
describeThingType pThingTypeName_ =
    DescribeThingType'
    { _dThingTypeName = pThingTypeName_
    }

-- | The name of the thing type.
dThingTypeName :: Lens' DescribeThingType Text
dThingTypeName = lens _dThingTypeName (\ s a -> s{_dThingTypeName = a});

instance AWSRequest DescribeThingType where
        type Rs DescribeThingType = DescribeThingTypeResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 DescribeThingTypeResponse' <$>
                   (x .?> "thingTypeProperties") <*>
                     (x .?> "thingTypeName")
                     <*> (x .?> "thingTypeMetadata")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeThingType

instance NFData DescribeThingType

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
    { _drsThingTypeProperties :: !(Maybe ThingTypeProperties)
    , _drsThingTypeName       :: !(Maybe Text)
    , _drsThingTypeMetadata   :: !(Maybe ThingTypeMetadata)
    , _drsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeThingTypeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsThingTypeProperties' - The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
--
-- * 'drsThingTypeName' - The name of the thing type.
--
-- * 'drsThingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
--
-- * 'drsResponseStatus' - -- | The response status code.
describeThingTypeResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DescribeThingTypeResponse
describeThingTypeResponse pResponseStatus_ =
    DescribeThingTypeResponse'
    { _drsThingTypeProperties = Nothing
    , _drsThingTypeName = Nothing
    , _drsThingTypeMetadata = Nothing
    , _drsResponseStatus = pResponseStatus_
    }

-- | The ThingTypeProperties contains information about the thing type including description, and a list of searchable thing attribute names.
drsThingTypeProperties :: Lens' DescribeThingTypeResponse (Maybe ThingTypeProperties)
drsThingTypeProperties = lens _drsThingTypeProperties (\ s a -> s{_drsThingTypeProperties = a});

-- | The name of the thing type.
drsThingTypeName :: Lens' DescribeThingTypeResponse (Maybe Text)
drsThingTypeName = lens _drsThingTypeName (\ s a -> s{_drsThingTypeName = a});

-- | The ThingTypeMetadata contains additional information about the thing type including: creation date and time, a value indicating whether the thing type is deprecated, and a date and time when it was deprecated.
drsThingTypeMetadata :: Lens' DescribeThingTypeResponse (Maybe ThingTypeMetadata)
drsThingTypeMetadata = lens _drsThingTypeMetadata (\ s a -> s{_drsThingTypeMetadata = a});

-- | -- | The response status code.
drsResponseStatus :: Lens' DescribeThingTypeResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a});

instance NFData DescribeThingTypeResponse
