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
-- Module      : Amazonka.FMS.PutNotificationChannel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Designates the IAM role and Amazon Simple Notification Service (SNS)
-- topic that Firewall Manager uses to record SNS logs.
--
-- To perform this action outside of the console, you must configure the
-- SNS topic to allow the Firewall Manager role @AWSServiceRoleForFMS@ to
-- publish SNS logs. For more information, see
-- <https://docs.aws.amazon.com/waf/latest/developerguide/fms-api-permissions-ref.html Firewall Manager required permissions for API actions>
-- in the /Firewall Manager Developer Guide/.
module Amazonka.FMS.PutNotificationChannel
  ( -- * Creating a Request
    PutNotificationChannel (..),
    newPutNotificationChannel,

    -- * Request Lenses
    putNotificationChannel_snsTopicArn,
    putNotificationChannel_snsRoleName,

    -- * Destructuring the Response
    PutNotificationChannelResponse (..),
    newPutNotificationChannelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FMS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutNotificationChannel' smart constructor.
data PutNotificationChannel = PutNotificationChannel'
  { -- | The Amazon Resource Name (ARN) of the SNS topic that collects
    -- notifications from Firewall Manager.
    snsTopicArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to
    -- record Firewall Manager activity.
    snsRoleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutNotificationChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsTopicArn', 'putNotificationChannel_snsTopicArn' - The Amazon Resource Name (ARN) of the SNS topic that collects
-- notifications from Firewall Manager.
--
-- 'snsRoleName', 'putNotificationChannel_snsRoleName' - The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to
-- record Firewall Manager activity.
newPutNotificationChannel ::
  -- | 'snsTopicArn'
  Prelude.Text ->
  -- | 'snsRoleName'
  Prelude.Text ->
  PutNotificationChannel
newPutNotificationChannel pSnsTopicArn_ pSnsRoleName_ =
  PutNotificationChannel'
    { snsTopicArn =
        pSnsTopicArn_,
      snsRoleName = pSnsRoleName_
    }

-- | The Amazon Resource Name (ARN) of the SNS topic that collects
-- notifications from Firewall Manager.
putNotificationChannel_snsTopicArn :: Lens.Lens' PutNotificationChannel Prelude.Text
putNotificationChannel_snsTopicArn = Lens.lens (\PutNotificationChannel' {snsTopicArn} -> snsTopicArn) (\s@PutNotificationChannel' {} a -> s {snsTopicArn = a} :: PutNotificationChannel)

-- | The Amazon Resource Name (ARN) of the IAM role that allows Amazon SNS to
-- record Firewall Manager activity.
putNotificationChannel_snsRoleName :: Lens.Lens' PutNotificationChannel Prelude.Text
putNotificationChannel_snsRoleName = Lens.lens (\PutNotificationChannel' {snsRoleName} -> snsRoleName) (\s@PutNotificationChannel' {} a -> s {snsRoleName = a} :: PutNotificationChannel)

instance Core.AWSRequest PutNotificationChannel where
  type
    AWSResponse PutNotificationChannel =
      PutNotificationChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      PutNotificationChannelResponse'

instance Prelude.Hashable PutNotificationChannel where
  hashWithSalt _salt PutNotificationChannel' {..} =
    _salt `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` snsRoleName

instance Prelude.NFData PutNotificationChannel where
  rnf PutNotificationChannel' {..} =
    Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf snsRoleName

instance Data.ToHeaders PutNotificationChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSFMS_20180101.PutNotificationChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutNotificationChannel where
  toJSON PutNotificationChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("SnsTopicArn" Data..= snsTopicArn),
            Prelude.Just ("SnsRoleName" Data..= snsRoleName)
          ]
      )

instance Data.ToPath PutNotificationChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery PutNotificationChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutNotificationChannelResponse' smart constructor.
data PutNotificationChannelResponse = PutNotificationChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutNotificationChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutNotificationChannelResponse ::
  PutNotificationChannelResponse
newPutNotificationChannelResponse =
  PutNotificationChannelResponse'

instance
  Prelude.NFData
    PutNotificationChannelResponse
  where
  rnf _ = ()
