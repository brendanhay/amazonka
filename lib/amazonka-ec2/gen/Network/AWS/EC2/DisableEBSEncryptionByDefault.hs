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
-- Module      : Network.AWS.EC2.DisableEBSEncryptionByDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables EBS encryption by default for your account in the current Region.
--
--
-- After you disable encryption by default, you can still create encrypted volumes by enabling encryption when you create each volume.
--
-- Disabling encryption by default does not change the encryption status of your existing volumes.
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSEncryption.html Amazon EBS Encryption> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.DisableEBSEncryptionByDefault
  ( -- * Creating a Request
    disableEBSEncryptionByDefault,
    DisableEBSEncryptionByDefault,

    -- * Request Lenses
    deebdDryRun,

    -- * Destructuring the Response
    disableEBSEncryptionByDefaultResponse,
    DisableEBSEncryptionByDefaultResponse,

    -- * Response Lenses
    deebdrsEBSEncryptionByDefault,
    deebdrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableEBSEncryptionByDefault' smart constructor.
newtype DisableEBSEncryptionByDefault = DisableEBSEncryptionByDefault'
  { _deebdDryRun ::
      Maybe Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableEBSEncryptionByDefault' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deebdDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
disableEBSEncryptionByDefault ::
  DisableEBSEncryptionByDefault
disableEBSEncryptionByDefault =
  DisableEBSEncryptionByDefault' {_deebdDryRun = Nothing}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
deebdDryRun :: Lens' DisableEBSEncryptionByDefault (Maybe Bool)
deebdDryRun = lens _deebdDryRun (\s a -> s {_deebdDryRun = a})

instance AWSRequest DisableEBSEncryptionByDefault where
  type
    Rs DisableEBSEncryptionByDefault =
      DisableEBSEncryptionByDefaultResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DisableEBSEncryptionByDefaultResponse'
            <$> (x .@? "ebsEncryptionByDefault") <*> (pure (fromEnum s))
      )

instance Hashable DisableEBSEncryptionByDefault

instance NFData DisableEBSEncryptionByDefault

instance ToHeaders DisableEBSEncryptionByDefault where
  toHeaders = const mempty

instance ToPath DisableEBSEncryptionByDefault where
  toPath = const "/"

instance ToQuery DisableEBSEncryptionByDefault where
  toQuery DisableEBSEncryptionByDefault' {..} =
    mconcat
      [ "Action" =: ("DisableEbsEncryptionByDefault" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _deebdDryRun
      ]

-- | /See:/ 'disableEBSEncryptionByDefaultResponse' smart constructor.
data DisableEBSEncryptionByDefaultResponse = DisableEBSEncryptionByDefaultResponse'
  { _deebdrsEBSEncryptionByDefault ::
      !(Maybe Bool),
    _deebdrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableEBSEncryptionByDefaultResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'deebdrsEBSEncryptionByDefault' - The updated status of encryption by default.
--
-- * 'deebdrsResponseStatus' - -- | The response status code.
disableEBSEncryptionByDefaultResponse ::
  -- | 'deebdrsResponseStatus'
  Int ->
  DisableEBSEncryptionByDefaultResponse
disableEBSEncryptionByDefaultResponse pResponseStatus_ =
  DisableEBSEncryptionByDefaultResponse'
    { _deebdrsEBSEncryptionByDefault =
        Nothing,
      _deebdrsResponseStatus = pResponseStatus_
    }

-- | The updated status of encryption by default.
deebdrsEBSEncryptionByDefault :: Lens' DisableEBSEncryptionByDefaultResponse (Maybe Bool)
deebdrsEBSEncryptionByDefault = lens _deebdrsEBSEncryptionByDefault (\s a -> s {_deebdrsEBSEncryptionByDefault = a})

-- | -- | The response status code.
deebdrsResponseStatus :: Lens' DisableEBSEncryptionByDefaultResponse Int
deebdrsResponseStatus = lens _deebdrsResponseStatus (\s a -> s {_deebdrsResponseStatus = a})

instance NFData DisableEBSEncryptionByDefaultResponse
