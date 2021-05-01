{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a platform application object for one of the supported push
-- notification services, such as APNS and GCM (Firebase Cloud Messaging).
-- For more information, see
-- <https://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
module Network.AWS.SNS.DeletePlatformApplication
  ( -- * Creating a Request
    DeletePlatformApplication (..),
    newDeletePlatformApplication,

    -- * Request Lenses
    deletePlatformApplication_platformApplicationArn,

    -- * Destructuring the Response
    DeletePlatformApplicationResponse (..),
    newDeletePlatformApplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SNS.Types

-- | Input for DeletePlatformApplication action.
--
-- /See:/ 'newDeletePlatformApplication' smart constructor.
data DeletePlatformApplication = DeletePlatformApplication'
  { -- | PlatformApplicationArn of platform application object to delete.
    platformApplicationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePlatformApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'platformApplicationArn', 'deletePlatformApplication_platformApplicationArn' - PlatformApplicationArn of platform application object to delete.
newDeletePlatformApplication ::
  -- | 'platformApplicationArn'
  Prelude.Text ->
  DeletePlatformApplication
newDeletePlatformApplication pPlatformApplicationArn_ =
  DeletePlatformApplication'
    { platformApplicationArn =
        pPlatformApplicationArn_
    }

-- | PlatformApplicationArn of platform application object to delete.
deletePlatformApplication_platformApplicationArn :: Lens.Lens' DeletePlatformApplication Prelude.Text
deletePlatformApplication_platformApplicationArn = Lens.lens (\DeletePlatformApplication' {platformApplicationArn} -> platformApplicationArn) (\s@DeletePlatformApplication' {} a -> s {platformApplicationArn = a} :: DeletePlatformApplication)

instance Prelude.AWSRequest DeletePlatformApplication where
  type
    Rs DeletePlatformApplication =
      DeletePlatformApplicationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeletePlatformApplicationResponse'

instance Prelude.Hashable DeletePlatformApplication

instance Prelude.NFData DeletePlatformApplication

instance Prelude.ToHeaders DeletePlatformApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeletePlatformApplication where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeletePlatformApplication where
  toQuery DeletePlatformApplication' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeletePlatformApplication" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-03-31" :: Prelude.ByteString),
        "PlatformApplicationArn"
          Prelude.=: platformApplicationArn
      ]

-- | /See:/ 'newDeletePlatformApplicationResponse' smart constructor.
data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePlatformApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePlatformApplicationResponse ::
  DeletePlatformApplicationResponse
newDeletePlatformApplicationResponse =
  DeletePlatformApplicationResponse'

instance
  Prelude.NFData
    DeletePlatformApplicationResponse
