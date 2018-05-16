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
-- Module      : Network.AWS.DeviceFarm.CreateInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a profile that can be applied to one or more private fleet device instances.
--
--
module Network.AWS.DeviceFarm.CreateInstanceProfile
    (
    -- * Creating a Request
      createInstanceProfile
    , CreateInstanceProfile
    -- * Request Lenses
    , cipRebootAfterUse
    , cipPackageCleanup
    , cipExcludeAppPackagesFromCleanup
    , cipDescription
    , cipName

    -- * Destructuring the Response
    , createInstanceProfileResponse
    , CreateInstanceProfileResponse
    -- * Response Lenses
    , ciprsInstanceProfile
    , ciprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createInstanceProfile' smart constructor.
data CreateInstanceProfile = CreateInstanceProfile'
  { _cipRebootAfterUse                :: !(Maybe Bool)
  , _cipPackageCleanup                :: !(Maybe Bool)
  , _cipExcludeAppPackagesFromCleanup :: !(Maybe [Text])
  , _cipDescription                   :: !(Maybe Text)
  , _cipName                          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cipRebootAfterUse' - When set to @true@ , Device Farm will reboot the instance after a test run. The default value is @true@ .
--
-- * 'cipPackageCleanup' - When set to @true@ , Device Farm will remove app packages after a test run. The default value is @false@ for private devices.
--
-- * 'cipExcludeAppPackagesFromCleanup' - An array of strings specifying the list of app packages that should not be cleaned up from the device after a test run is over. The list of packages is only considered if you set @packageCleanup@ to @true@ .
--
-- * 'cipDescription' - The description of your instance profile.
--
-- * 'cipName' - The name of your instance profile.
createInstanceProfile
    :: Text -- ^ 'cipName'
    -> CreateInstanceProfile
createInstanceProfile pName_ =
  CreateInstanceProfile'
    { _cipRebootAfterUse = Nothing
    , _cipPackageCleanup = Nothing
    , _cipExcludeAppPackagesFromCleanup = Nothing
    , _cipDescription = Nothing
    , _cipName = pName_
    }


-- | When set to @true@ , Device Farm will reboot the instance after a test run. The default value is @true@ .
cipRebootAfterUse :: Lens' CreateInstanceProfile (Maybe Bool)
cipRebootAfterUse = lens _cipRebootAfterUse (\ s a -> s{_cipRebootAfterUse = a})

-- | When set to @true@ , Device Farm will remove app packages after a test run. The default value is @false@ for private devices.
cipPackageCleanup :: Lens' CreateInstanceProfile (Maybe Bool)
cipPackageCleanup = lens _cipPackageCleanup (\ s a -> s{_cipPackageCleanup = a})

-- | An array of strings specifying the list of app packages that should not be cleaned up from the device after a test run is over. The list of packages is only considered if you set @packageCleanup@ to @true@ .
cipExcludeAppPackagesFromCleanup :: Lens' CreateInstanceProfile [Text]
cipExcludeAppPackagesFromCleanup = lens _cipExcludeAppPackagesFromCleanup (\ s a -> s{_cipExcludeAppPackagesFromCleanup = a}) . _Default . _Coerce

-- | The description of your instance profile.
cipDescription :: Lens' CreateInstanceProfile (Maybe Text)
cipDescription = lens _cipDescription (\ s a -> s{_cipDescription = a})

-- | The name of your instance profile.
cipName :: Lens' CreateInstanceProfile Text
cipName = lens _cipName (\ s a -> s{_cipName = a})

instance AWSRequest CreateInstanceProfile where
        type Rs CreateInstanceProfile =
             CreateInstanceProfileResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 CreateInstanceProfileResponse' <$>
                   (x .?> "instanceProfile") <*> (pure (fromEnum s)))

instance Hashable CreateInstanceProfile where

instance NFData CreateInstanceProfile where

instance ToHeaders CreateInstanceProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.CreateInstanceProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateInstanceProfile where
        toJSON CreateInstanceProfile'{..}
          = object
              (catMaybes
                 [("rebootAfterUse" .=) <$> _cipRebootAfterUse,
                  ("packageCleanup" .=) <$> _cipPackageCleanup,
                  ("excludeAppPackagesFromCleanup" .=) <$>
                    _cipExcludeAppPackagesFromCleanup,
                  ("description" .=) <$> _cipDescription,
                  Just ("name" .= _cipName)])

instance ToPath CreateInstanceProfile where
        toPath = const "/"

instance ToQuery CreateInstanceProfile where
        toQuery = const mempty

-- | /See:/ 'createInstanceProfileResponse' smart constructor.
data CreateInstanceProfileResponse = CreateInstanceProfileResponse'
  { _ciprsInstanceProfile :: !(Maybe InstanceProfile)
  , _ciprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ciprsInstanceProfile' - An object containing information about your instance profile.
--
-- * 'ciprsResponseStatus' - -- | The response status code.
createInstanceProfileResponse
    :: Int -- ^ 'ciprsResponseStatus'
    -> CreateInstanceProfileResponse
createInstanceProfileResponse pResponseStatus_ =
  CreateInstanceProfileResponse'
    {_ciprsInstanceProfile = Nothing, _ciprsResponseStatus = pResponseStatus_}


-- | An object containing information about your instance profile.
ciprsInstanceProfile :: Lens' CreateInstanceProfileResponse (Maybe InstanceProfile)
ciprsInstanceProfile = lens _ciprsInstanceProfile (\ s a -> s{_ciprsInstanceProfile = a})

-- | -- | The response status code.
ciprsResponseStatus :: Lens' CreateInstanceProfileResponse Int
ciprsResponseStatus = lens _ciprsResponseStatus (\ s a -> s{_ciprsResponseStatus = a})

instance NFData CreateInstanceProfileResponse where
