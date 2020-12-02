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
-- Module      : Network.AWS.EC2.GetEBSEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes whether EBS encryption by default is enabled for your account in the current Region.
--
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.GetEBSEncryptionByDefault
  ( -- * Creating a Request
    getEBSEncryptionByDefault,
    GetEBSEncryptionByDefault,

    -- * Request Lenses
    geebdDryRun,

    -- * Destructuring the Response
    getEBSEncryptionByDefaultResponse,
    GetEBSEncryptionByDefaultResponse,

    -- * Response Lenses
    geebdrsEBSEncryptionByDefault,
    geebdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getEBSEncryptionByDefault' smart constructor.
newtype GetEBSEncryptionByDefault = GetEBSEncryptionByDefault'
  { _geebdDryRun ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetEBSEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geebdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
getEBSEncryptionByDefault ::
  GetEBSEncryptionByDefault
getEBSEncryptionByDefault =
  GetEBSEncryptionByDefault' {_geebdDryRun = Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
geebdDryRun :: Lens' GetEBSEncryptionByDefault (Maybe Bool)
geebdDryRun = lens _geebdDryRun (\s a -> s {_geebdDryRun = a})

instance AWSRequest GetEBSEncryptionByDefault where
  type
    Rs GetEBSEncryptionByDefault =
      GetEBSEncryptionByDefaultResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          GetEBSEncryptionByDefaultResponse'
            <$> (x .@? "ebsEncryptionByDefault") <*> (pure (fromEnum s))
      )

instance Hashable GetEBSEncryptionByDefault

instance NFData GetEBSEncryptionByDefault

instance ToHeaders GetEBSEncryptionByDefault where
  toHeaders = const mempty

instance ToPath GetEBSEncryptionByDefault where
  toPath = const "/"

instance ToQuery GetEBSEncryptionByDefault where
  toQuery GetEBSEncryptionByDefault' {..} =
    mconcat
      [ "Action" =: ("GetEbsEncryptionByDefault" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _geebdDryRun
      ]

-- | /See:/ 'getEBSEncryptionByDefaultResponse' smart constructor.
data GetEBSEncryptionByDefaultResponse = GetEBSEncryptionByDefaultResponse'
  { _geebdrsEBSEncryptionByDefault ::
      !(Maybe Bool),
    _geebdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetEBSEncryptionByDefaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'geebdrsEBSEncryptionByDefault' - Indicates whether encryption by default is enabled.
--
-- * 'geebdrsResponseStatus' - -- | The response status code.
getEBSEncryptionByDefaultResponse ::
  -- | 'geebdrsResponseStatus'
  Int ->
  GetEBSEncryptionByDefaultResponse
getEBSEncryptionByDefaultResponse pResponseStatus_ =
  GetEBSEncryptionByDefaultResponse'
    { _geebdrsEBSEncryptionByDefault =
        Nothing,
      _geebdrsResponseStatus = pResponseStatus_
    }

-- | Indicates whether encryption by default is enabled.
geebdrsEBSEncryptionByDefault :: Lens' GetEBSEncryptionByDefaultResponse (Maybe Bool)
geebdrsEBSEncryptionByDefault = lens _geebdrsEBSEncryptionByDefault (\s a -> s {_geebdrsEBSEncryptionByDefault = a})

-- | -- | The response status code.
geebdrsResponseStatus :: Lens' GetEBSEncryptionByDefaultResponse Int
geebdrsResponseStatus = lens _geebdrsResponseStatus (\s a -> s {_geebdrsResponseStatus = a})

instance NFData GetEBSEncryptionByDefaultResponse
