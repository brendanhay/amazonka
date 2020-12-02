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
-- Module      : Network.AWS.EC2.EnableEBSEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables EBS encryption by default for your account in the current Region.
--
--
-- After you enable encryption by default, the EBS volumes that you create are are always encrypted, either using the default CMK or the CMK that you specified when you created each volume. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- You can specify the default CMK for encryption by default using 'ModifyEbsDefaultKmsKeyId' or 'ResetEbsDefaultKmsKeyId' .
--
-- Enabling encryption by default has no effect on the encryption status of your existing volumes.
--
-- After you enable encryption by default, you can no longer launch instances using instance types that do not support encryption. For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html#EBSEncryption_supported_instances Supported instance types> .
module Network.AWS.EC2.EnableEBSEncryptionByDefault
  ( -- * Creating a Request
    enableEBSEncryptionByDefault,
    EnableEBSEncryptionByDefault,

    -- * Request Lenses
    eeebdDryRun,

    -- * Destructuring the Response
    enableEBSEncryptionByDefaultResponse,
    EnableEBSEncryptionByDefaultResponse,

    -- * Response Lenses
    eeebdrsEBSEncryptionByDefault,
    eeebdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableEBSEncryptionByDefault' smart constructor.
newtype EnableEBSEncryptionByDefault = EnableEBSEncryptionByDefault'
  { _eeebdDryRun ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableEBSEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eeebdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
enableEBSEncryptionByDefault ::
  EnableEBSEncryptionByDefault
enableEBSEncryptionByDefault =
  EnableEBSEncryptionByDefault' {_eeebdDryRun = Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
eeebdDryRun :: Lens' EnableEBSEncryptionByDefault (Maybe Bool)
eeebdDryRun = lens _eeebdDryRun (\s a -> s {_eeebdDryRun = a})

instance AWSRequest EnableEBSEncryptionByDefault where
  type
    Rs EnableEBSEncryptionByDefault =
      EnableEBSEncryptionByDefaultResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          EnableEBSEncryptionByDefaultResponse'
            <$> (x .@? "ebsEncryptionByDefault") <*> (pure (fromEnum s))
      )

instance Hashable EnableEBSEncryptionByDefault

instance NFData EnableEBSEncryptionByDefault

instance ToHeaders EnableEBSEncryptionByDefault where
  toHeaders = const mempty

instance ToPath EnableEBSEncryptionByDefault where
  toPath = const "/"

instance ToQuery EnableEBSEncryptionByDefault where
  toQuery EnableEBSEncryptionByDefault' {..} =
    mconcat
      [ "Action" =: ("EnableEbsEncryptionByDefault" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _eeebdDryRun
      ]

-- | /See:/ 'enableEBSEncryptionByDefaultResponse' smart constructor.
data EnableEBSEncryptionByDefaultResponse = EnableEBSEncryptionByDefaultResponse'
  { _eeebdrsEBSEncryptionByDefault ::
      !(Maybe Bool),
    _eeebdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableEBSEncryptionByDefaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eeebdrsEBSEncryptionByDefault' - The updated status of encryption by default.
--
-- * 'eeebdrsResponseStatus' - -- | The response status code.
enableEBSEncryptionByDefaultResponse ::
  -- | 'eeebdrsResponseStatus'
  Int ->
  EnableEBSEncryptionByDefaultResponse
enableEBSEncryptionByDefaultResponse pResponseStatus_ =
  EnableEBSEncryptionByDefaultResponse'
    { _eeebdrsEBSEncryptionByDefault =
        Nothing,
      _eeebdrsResponseStatus = pResponseStatus_
    }

-- | The updated status of encryption by default.
eeebdrsEBSEncryptionByDefault :: Lens' EnableEBSEncryptionByDefaultResponse (Maybe Bool)
eeebdrsEBSEncryptionByDefault = lens _eeebdrsEBSEncryptionByDefault (\s a -> s {_eeebdrsEBSEncryptionByDefault = a})

-- | -- | The response status code.
eeebdrsResponseStatus :: Lens' EnableEBSEncryptionByDefaultResponse Int
eeebdrsResponseStatus = lens _eeebdrsResponseStatus (\s a -> s {_eeebdrsResponseStatus = a})

instance NFData EnableEBSEncryptionByDefaultResponse
