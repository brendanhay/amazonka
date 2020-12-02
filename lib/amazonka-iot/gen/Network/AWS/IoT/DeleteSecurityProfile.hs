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
-- Module      : Network.AWS.IoT.DeleteSecurityProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a Device Defender security profile.
module Network.AWS.IoT.DeleteSecurityProfile
  ( -- * Creating a Request
    deleteSecurityProfile,
    DeleteSecurityProfile,

    -- * Request Lenses
    dspExpectedVersion,
    dspSecurityProfileName,

    -- * Destructuring the Response
    deleteSecurityProfileResponse,
    DeleteSecurityProfileResponse,

    -- * Response Lenses
    dsprsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteSecurityProfile' smart constructor.
data DeleteSecurityProfile = DeleteSecurityProfile'
  { _dspExpectedVersion ::
      !(Maybe Integer),
    _dspSecurityProfileName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSecurityProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dspExpectedVersion' - The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
--
-- * 'dspSecurityProfileName' - The name of the security profile to be deleted.
deleteSecurityProfile ::
  -- | 'dspSecurityProfileName'
  Text ->
  DeleteSecurityProfile
deleteSecurityProfile pSecurityProfileName_ =
  DeleteSecurityProfile'
    { _dspExpectedVersion = Nothing,
      _dspSecurityProfileName = pSecurityProfileName_
    }

-- | The expected version of the security profile. A new version is generated whenever the security profile is updated. If you specify a value that is different from the actual version, a @VersionConflictException@ is thrown.
dspExpectedVersion :: Lens' DeleteSecurityProfile (Maybe Integer)
dspExpectedVersion = lens _dspExpectedVersion (\s a -> s {_dspExpectedVersion = a})

-- | The name of the security profile to be deleted.
dspSecurityProfileName :: Lens' DeleteSecurityProfile Text
dspSecurityProfileName = lens _dspSecurityProfileName (\s a -> s {_dspSecurityProfileName = a})

instance AWSRequest DeleteSecurityProfile where
  type Rs DeleteSecurityProfile = DeleteSecurityProfileResponse
  request = delete ioT
  response =
    receiveEmpty
      (\s h x -> DeleteSecurityProfileResponse' <$> (pure (fromEnum s)))

instance Hashable DeleteSecurityProfile

instance NFData DeleteSecurityProfile

instance ToHeaders DeleteSecurityProfile where
  toHeaders = const mempty

instance ToPath DeleteSecurityProfile where
  toPath DeleteSecurityProfile' {..} =
    mconcat ["/security-profiles/", toBS _dspSecurityProfileName]

instance ToQuery DeleteSecurityProfile where
  toQuery DeleteSecurityProfile' {..} =
    mconcat ["expectedVersion" =: _dspExpectedVersion]

-- | /See:/ 'deleteSecurityProfileResponse' smart constructor.
newtype DeleteSecurityProfileResponse = DeleteSecurityProfileResponse'
  { _dsprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteSecurityProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsprsResponseStatus' - -- | The response status code.
deleteSecurityProfileResponse ::
  -- | 'dsprsResponseStatus'
  Int ->
  DeleteSecurityProfileResponse
deleteSecurityProfileResponse pResponseStatus_ =
  DeleteSecurityProfileResponse'
    { _dsprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
dsprsResponseStatus :: Lens' DeleteSecurityProfileResponse Int
dsprsResponseStatus = lens _dsprsResponseStatus (\s a -> s {_dsprsResponseStatus = a})

instance NFData DeleteSecurityProfileResponse
