{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.CreateDevicePool
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a device pool.
--
-- <http://docs.aws.amazon.com/devicefarm/latest/APIReference/API_CreateDevicePool.html>
module Network.AWS.DeviceFarm.CreateDevicePool
    (
    -- * Request
      CreateDevicePool
    -- ** Request constructor
    , createDevicePool
    -- ** Request lenses
    , cdprqDescription
    , cdprqProjectARN
    , cdprqName
    , cdprqRules

    -- * Response
    , CreateDevicePoolResponse
    -- ** Response constructor
    , createDevicePoolResponse
    -- ** Response lenses
    , cdprsDevicePool
    , cdprsStatus
    ) where

import           Network.AWS.DeviceFarm.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents a request to the create device pool operation.
--
-- /See:/ 'createDevicePool' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdprqDescription'
--
-- * 'cdprqProjectARN'
--
-- * 'cdprqName'
--
-- * 'cdprqRules'
data CreateDevicePool = CreateDevicePool'
    { _cdprqDescription :: !(Maybe Text)
    , _cdprqProjectARN  :: !Text
    , _cdprqName        :: !Text
    , _cdprqRules       :: ![Rule]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDevicePool' smart constructor.
createDevicePool :: Text -> Text -> CreateDevicePool
createDevicePool pProjectARN_ pName_ =
    CreateDevicePool'
    { _cdprqDescription = Nothing
    , _cdprqProjectARN = pProjectARN_
    , _cdprqName = pName_
    , _cdprqRules = mempty
    }

-- | The device pool\'s description.
cdprqDescription :: Lens' CreateDevicePool (Maybe Text)
cdprqDescription = lens _cdprqDescription (\ s a -> s{_cdprqDescription = a});

-- | The ARN of the project for the device pool.
cdprqProjectARN :: Lens' CreateDevicePool Text
cdprqProjectARN = lens _cdprqProjectARN (\ s a -> s{_cdprqProjectARN = a});

-- | The device pool\'s name.
cdprqName :: Lens' CreateDevicePool Text
cdprqName = lens _cdprqName (\ s a -> s{_cdprqName = a});

-- | The device pool\'s rules.
cdprqRules :: Lens' CreateDevicePool [Rule]
cdprqRules = lens _cdprqRules (\ s a -> s{_cdprqRules = a});

instance AWSRequest CreateDevicePool where
        type Sv CreateDevicePool = DeviceFarm
        type Rs CreateDevicePool = CreateDevicePoolResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 CreateDevicePoolResponse' <$>
                   (x .?> "devicePool") <*> (pure (fromEnum s)))

instance ToHeaders CreateDevicePool where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateDevicePool" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateDevicePool where
        toJSON CreateDevicePool'{..}
          = object
              ["description" .= _cdprqDescription,
               "projectArn" .= _cdprqProjectARN,
               "name" .= _cdprqName, "rules" .= _cdprqRules]

instance ToPath CreateDevicePool where
        toPath = const "/"

instance ToQuery CreateDevicePool where
        toQuery = const mempty

-- | Represents the result of a create device pool request.
--
-- /See:/ 'createDevicePoolResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdprsDevicePool'
--
-- * 'cdprsStatus'
data CreateDevicePoolResponse = CreateDevicePoolResponse'
    { _cdprsDevicePool :: !(Maybe DevicePool)
    , _cdprsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDevicePoolResponse' smart constructor.
createDevicePoolResponse :: Int -> CreateDevicePoolResponse
createDevicePoolResponse pStatus_ =
    CreateDevicePoolResponse'
    { _cdprsDevicePool = Nothing
    , _cdprsStatus = pStatus_
    }

-- | The newly created device pool.
cdprsDevicePool :: Lens' CreateDevicePoolResponse (Maybe DevicePool)
cdprsDevicePool = lens _cdprsDevicePool (\ s a -> s{_cdprsDevicePool = a});

-- | FIXME: Undocumented member.
cdprsStatus :: Lens' CreateDevicePoolResponse Int
cdprsStatus = lens _cdprsStatus (\ s a -> s{_cdprsStatus = a});
