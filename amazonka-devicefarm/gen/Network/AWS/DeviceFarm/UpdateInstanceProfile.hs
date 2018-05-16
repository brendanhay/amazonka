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
-- Module      : Network.AWS.DeviceFarm.UpdateInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about an existing private device instance profile.
--
--
module Network.AWS.DeviceFarm.UpdateInstanceProfile
    (
    -- * Creating a Request
      updateInstanceProfile
    , UpdateInstanceProfile
    -- * Request Lenses
    , uipRebootAfterUse
    , uipName
    , uipPackageCleanup
    , uipExcludeAppPackagesFromCleanup
    , uipDescription
    , uipArn

    -- * Destructuring the Response
    , updateInstanceProfileResponse
    , UpdateInstanceProfileResponse
    -- * Response Lenses
    , uiprsInstanceProfile
    , uiprsResponseStatus
    ) where

import Network.AWS.DeviceFarm.Types
import Network.AWS.DeviceFarm.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateInstanceProfile' smart constructor.
data UpdateInstanceProfile = UpdateInstanceProfile'
  { _uipRebootAfterUse                :: !(Maybe Bool)
  , _uipName                          :: !(Maybe Text)
  , _uipPackageCleanup                :: !(Maybe Bool)
  , _uipExcludeAppPackagesFromCleanup :: !(Maybe [Text])
  , _uipDescription                   :: !(Maybe Text)
  , _uipArn                           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uipRebootAfterUse' - The updated choice for whether you want to reboot the device after use. The default value is @true@ .
--
-- * 'uipName' - The updated name for your instance profile.
--
-- * 'uipPackageCleanup' - The updated choice for whether you want to specify package cleanup. The default value is @false@ for private devices.
--
-- * 'uipExcludeAppPackagesFromCleanup' - An array of strings specifying the list of app packages that should not be cleaned up from the device after a test run is over. The list of packages is only considered if you set @packageCleanup@ to @true@ .
--
-- * 'uipDescription' - The updated description for your instance profile.
--
-- * 'uipArn' - The Amazon Resource Name (ARN) of the instance profile.
updateInstanceProfile
    :: Text -- ^ 'uipArn'
    -> UpdateInstanceProfile
updateInstanceProfile pArn_ =
  UpdateInstanceProfile'
    { _uipRebootAfterUse = Nothing
    , _uipName = Nothing
    , _uipPackageCleanup = Nothing
    , _uipExcludeAppPackagesFromCleanup = Nothing
    , _uipDescription = Nothing
    , _uipArn = pArn_
    }


-- | The updated choice for whether you want to reboot the device after use. The default value is @true@ .
uipRebootAfterUse :: Lens' UpdateInstanceProfile (Maybe Bool)
uipRebootAfterUse = lens _uipRebootAfterUse (\ s a -> s{_uipRebootAfterUse = a})

-- | The updated name for your instance profile.
uipName :: Lens' UpdateInstanceProfile (Maybe Text)
uipName = lens _uipName (\ s a -> s{_uipName = a})

-- | The updated choice for whether you want to specify package cleanup. The default value is @false@ for private devices.
uipPackageCleanup :: Lens' UpdateInstanceProfile (Maybe Bool)
uipPackageCleanup = lens _uipPackageCleanup (\ s a -> s{_uipPackageCleanup = a})

-- | An array of strings specifying the list of app packages that should not be cleaned up from the device after a test run is over. The list of packages is only considered if you set @packageCleanup@ to @true@ .
uipExcludeAppPackagesFromCleanup :: Lens' UpdateInstanceProfile [Text]
uipExcludeAppPackagesFromCleanup = lens _uipExcludeAppPackagesFromCleanup (\ s a -> s{_uipExcludeAppPackagesFromCleanup = a}) . _Default . _Coerce

-- | The updated description for your instance profile.
uipDescription :: Lens' UpdateInstanceProfile (Maybe Text)
uipDescription = lens _uipDescription (\ s a -> s{_uipDescription = a})

-- | The Amazon Resource Name (ARN) of the instance profile.
uipArn :: Lens' UpdateInstanceProfile Text
uipArn = lens _uipArn (\ s a -> s{_uipArn = a})

instance AWSRequest UpdateInstanceProfile where
        type Rs UpdateInstanceProfile =
             UpdateInstanceProfileResponse
        request = postJSON deviceFarm
        response
          = receiveJSON
              (\ s h x ->
                 UpdateInstanceProfileResponse' <$>
                   (x .?> "instanceProfile") <*> (pure (fromEnum s)))

instance Hashable UpdateInstanceProfile where

instance NFData UpdateInstanceProfile where

instance ToHeaders UpdateInstanceProfile where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("DeviceFarm_20150623.UpdateInstanceProfile" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateInstanceProfile where
        toJSON UpdateInstanceProfile'{..}
          = object
              (catMaybes
                 [("rebootAfterUse" .=) <$> _uipRebootAfterUse,
                  ("name" .=) <$> _uipName,
                  ("packageCleanup" .=) <$> _uipPackageCleanup,
                  ("excludeAppPackagesFromCleanup" .=) <$>
                    _uipExcludeAppPackagesFromCleanup,
                  ("description" .=) <$> _uipDescription,
                  Just ("arn" .= _uipArn)])

instance ToPath UpdateInstanceProfile where
        toPath = const "/"

instance ToQuery UpdateInstanceProfile where
        toQuery = const mempty

-- | /See:/ 'updateInstanceProfileResponse' smart constructor.
data UpdateInstanceProfileResponse = UpdateInstanceProfileResponse'
  { _uiprsInstanceProfile :: !(Maybe InstanceProfile)
  , _uiprsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateInstanceProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uiprsInstanceProfile' - An object containing information about your instance profile.
--
-- * 'uiprsResponseStatus' - -- | The response status code.
updateInstanceProfileResponse
    :: Int -- ^ 'uiprsResponseStatus'
    -> UpdateInstanceProfileResponse
updateInstanceProfileResponse pResponseStatus_ =
  UpdateInstanceProfileResponse'
    {_uiprsInstanceProfile = Nothing, _uiprsResponseStatus = pResponseStatus_}


-- | An object containing information about your instance profile.
uiprsInstanceProfile :: Lens' UpdateInstanceProfileResponse (Maybe InstanceProfile)
uiprsInstanceProfile = lens _uiprsInstanceProfile (\ s a -> s{_uiprsInstanceProfile = a})

-- | -- | The response status code.
uiprsResponseStatus :: Lens' UpdateInstanceProfileResponse Int
uiprsResponseStatus = lens _uiprsResponseStatus (\ s a -> s{_uiprsResponseStatus = a})

instance NFData UpdateInstanceProfileResponse where
