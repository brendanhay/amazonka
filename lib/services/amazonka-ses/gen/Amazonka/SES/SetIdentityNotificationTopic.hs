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
-- Module      : Amazonka.SES.SetIdentityNotificationTopic
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an Amazon Simple Notification Service (Amazon SNS) topic to use
-- when delivering notifications. When you use this operation, you specify
-- a verified identity, such as an email address or domain. When you send
-- an email that uses the chosen identity in the Source field, Amazon SES
-- sends notifications to the topic you specified. You can send bounce,
-- complaint, or delivery notifications (or any combination of the three)
-- to the Amazon SNS topic that you specify.
--
-- You can execute this operation no more than once per second.
--
-- For more information about feedback notification, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications.html Amazon SES Developer Guide>.
module Amazonka.SES.SetIdentityNotificationTopic
  ( -- * Creating a Request
    SetIdentityNotificationTopic (..),
    newSetIdentityNotificationTopic,

    -- * Request Lenses
    setIdentityNotificationTopic_snsTopic,
    setIdentityNotificationTopic_identity,
    setIdentityNotificationTopic_notificationType,

    -- * Destructuring the Response
    SetIdentityNotificationTopicResponse (..),
    newSetIdentityNotificationTopicResponse,

    -- * Response Lenses
    setIdentityNotificationTopicResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SES.Types

-- | Represents a request to specify the Amazon SNS topic to which Amazon SES
-- will publish bounce, complaint, or delivery notifications for emails
-- sent with that identity as the Source. For information about Amazon SES
-- notifications, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/notifications-via-sns.html Amazon SES Developer Guide>.
--
-- /See:/ 'newSetIdentityNotificationTopic' smart constructor.
data SetIdentityNotificationTopic = SetIdentityNotificationTopic'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter
    -- is omitted from the request or a null value is passed, @SnsTopic@ is
    -- cleared and publishing is disabled.
    snsTopic :: Prelude.Maybe Prelude.Text,
    -- | The identity (email address or domain) that you want to set the Amazon
    -- SNS topic for.
    --
    -- You can only specify a verified identity for this parameter.
    --
    -- You can specify an identity by using its name or by using its Amazon
    -- Resource Name (ARN). The following examples are all valid identities:
    -- @sender\@example.com@, @example.com@,
    -- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
    identity :: Prelude.Text,
    -- | The type of notifications that will be published to the specified Amazon
    -- SNS topic.
    notificationType :: NotificationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityNotificationTopic' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'snsTopic', 'setIdentityNotificationTopic_snsTopic' - The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter
-- is omitted from the request or a null value is passed, @SnsTopic@ is
-- cleared and publishing is disabled.
--
-- 'identity', 'setIdentityNotificationTopic_identity' - The identity (email address or domain) that you want to set the Amazon
-- SNS topic for.
--
-- You can only specify a verified identity for this parameter.
--
-- You can specify an identity by using its name or by using its Amazon
-- Resource Name (ARN). The following examples are all valid identities:
-- @sender\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
--
-- 'notificationType', 'setIdentityNotificationTopic_notificationType' - The type of notifications that will be published to the specified Amazon
-- SNS topic.
newSetIdentityNotificationTopic ::
  -- | 'identity'
  Prelude.Text ->
  -- | 'notificationType'
  NotificationType ->
  SetIdentityNotificationTopic
newSetIdentityNotificationTopic
  pIdentity_
  pNotificationType_ =
    SetIdentityNotificationTopic'
      { snsTopic =
          Prelude.Nothing,
        identity = pIdentity_,
        notificationType = pNotificationType_
      }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic. If the parameter
-- is omitted from the request or a null value is passed, @SnsTopic@ is
-- cleared and publishing is disabled.
setIdentityNotificationTopic_snsTopic :: Lens.Lens' SetIdentityNotificationTopic (Prelude.Maybe Prelude.Text)
setIdentityNotificationTopic_snsTopic = Lens.lens (\SetIdentityNotificationTopic' {snsTopic} -> snsTopic) (\s@SetIdentityNotificationTopic' {} a -> s {snsTopic = a} :: SetIdentityNotificationTopic)

-- | The identity (email address or domain) that you want to set the Amazon
-- SNS topic for.
--
-- You can only specify a verified identity for this parameter.
--
-- You can specify an identity by using its name or by using its Amazon
-- Resource Name (ARN). The following examples are all valid identities:
-- @sender\@example.com@, @example.com@,
-- @arn:aws:ses:us-east-1:123456789012:identity\/example.com@.
setIdentityNotificationTopic_identity :: Lens.Lens' SetIdentityNotificationTopic Prelude.Text
setIdentityNotificationTopic_identity = Lens.lens (\SetIdentityNotificationTopic' {identity} -> identity) (\s@SetIdentityNotificationTopic' {} a -> s {identity = a} :: SetIdentityNotificationTopic)

-- | The type of notifications that will be published to the specified Amazon
-- SNS topic.
setIdentityNotificationTopic_notificationType :: Lens.Lens' SetIdentityNotificationTopic NotificationType
setIdentityNotificationTopic_notificationType = Lens.lens (\SetIdentityNotificationTopic' {notificationType} -> notificationType) (\s@SetIdentityNotificationTopic' {} a -> s {notificationType = a} :: SetIdentityNotificationTopic)

instance Core.AWSRequest SetIdentityNotificationTopic where
  type
    AWSResponse SetIdentityNotificationTopic =
      SetIdentityNotificationTopicResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "SetIdentityNotificationTopicResult"
      ( \s h x ->
          SetIdentityNotificationTopicResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    SetIdentityNotificationTopic
  where
  hashWithSalt _salt SetIdentityNotificationTopic' {..} =
    _salt
      `Prelude.hashWithSalt` snsTopic
      `Prelude.hashWithSalt` identity
      `Prelude.hashWithSalt` notificationType

instance Prelude.NFData SetIdentityNotificationTopic where
  rnf SetIdentityNotificationTopic' {..} =
    Prelude.rnf snsTopic
      `Prelude.seq` Prelude.rnf identity
      `Prelude.seq` Prelude.rnf notificationType

instance Data.ToHeaders SetIdentityNotificationTopic where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath SetIdentityNotificationTopic where
  toPath = Prelude.const "/"

instance Data.ToQuery SetIdentityNotificationTopic where
  toQuery SetIdentityNotificationTopic' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "SetIdentityNotificationTopic" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2010-12-01" :: Prelude.ByteString),
        "SnsTopic" Data.=: snsTopic,
        "Identity" Data.=: identity,
        "NotificationType" Data.=: notificationType
      ]

-- | An empty element returned on a successful request.
--
-- /See:/ 'newSetIdentityNotificationTopicResponse' smart constructor.
data SetIdentityNotificationTopicResponse = SetIdentityNotificationTopicResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetIdentityNotificationTopicResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'setIdentityNotificationTopicResponse_httpStatus' - The response's http status code.
newSetIdentityNotificationTopicResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetIdentityNotificationTopicResponse
newSetIdentityNotificationTopicResponse pHttpStatus_ =
  SetIdentityNotificationTopicResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
setIdentityNotificationTopicResponse_httpStatus :: Lens.Lens' SetIdentityNotificationTopicResponse Prelude.Int
setIdentityNotificationTopicResponse_httpStatus = Lens.lens (\SetIdentityNotificationTopicResponse' {httpStatus} -> httpStatus) (\s@SetIdentityNotificationTopicResponse' {} a -> s {httpStatus = a} :: SetIdentityNotificationTopicResponse)

instance
  Prelude.NFData
    SetIdentityNotificationTopicResponse
  where
  rnf SetIdentityNotificationTopicResponse' {..} =
    Prelude.rnf httpStatus
