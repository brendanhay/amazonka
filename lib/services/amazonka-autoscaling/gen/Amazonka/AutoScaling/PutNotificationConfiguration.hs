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
-- Module      : Amazonka.AutoScaling.PutNotificationConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures an Auto Scaling group to send notifications when specified
-- events take place. Subscribers to the specified topic can have messages
-- delivered to an endpoint such as a web server or an email address.
--
-- This configuration overwrites any existing configuration.
--
-- For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/ASGettingNotifications.html Getting Amazon SNS notifications when your Auto Scaling group scales>
-- in the /Amazon EC2 Auto Scaling User Guide/.
--
-- If you exceed your maximum limit of SNS topics, which is 10 per Auto
-- Scaling group, the call fails.
module Amazonka.AutoScaling.PutNotificationConfiguration
  ( -- * Creating a Request
    PutNotificationConfiguration (..),
    newPutNotificationConfiguration,

    -- * Request Lenses
    putNotificationConfiguration_autoScalingGroupName,
    putNotificationConfiguration_topicARN,
    putNotificationConfiguration_notificationTypes,

    -- * Destructuring the Response
    PutNotificationConfigurationResponse (..),
    newPutNotificationConfigurationResponse,
  )
where

import Amazonka.AutoScaling.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutNotificationConfiguration' smart constructor.
data PutNotificationConfiguration = PutNotificationConfiguration'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic.
    topicARN :: Prelude.Text,
    -- | The type of event that causes the notification to be sent. To query the
    -- notification types supported by Amazon EC2 Auto Scaling, call the
    -- DescribeAutoScalingNotificationTypes API.
    notificationTypes :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'putNotificationConfiguration_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'topicARN', 'putNotificationConfiguration_topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic.
--
-- 'notificationTypes', 'putNotificationConfiguration_notificationTypes' - The type of event that causes the notification to be sent. To query the
-- notification types supported by Amazon EC2 Auto Scaling, call the
-- DescribeAutoScalingNotificationTypes API.
newPutNotificationConfiguration ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'topicARN'
  Prelude.Text ->
  PutNotificationConfiguration
newPutNotificationConfiguration
  pAutoScalingGroupName_
  pTopicARN_ =
    PutNotificationConfiguration'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        topicARN = pTopicARN_,
        notificationTypes = Prelude.mempty
      }

-- | The name of the Auto Scaling group.
putNotificationConfiguration_autoScalingGroupName :: Lens.Lens' PutNotificationConfiguration Prelude.Text
putNotificationConfiguration_autoScalingGroupName = Lens.lens (\PutNotificationConfiguration' {autoScalingGroupName} -> autoScalingGroupName) (\s@PutNotificationConfiguration' {} a -> s {autoScalingGroupName = a} :: PutNotificationConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic.
putNotificationConfiguration_topicARN :: Lens.Lens' PutNotificationConfiguration Prelude.Text
putNotificationConfiguration_topicARN = Lens.lens (\PutNotificationConfiguration' {topicARN} -> topicARN) (\s@PutNotificationConfiguration' {} a -> s {topicARN = a} :: PutNotificationConfiguration)

-- | The type of event that causes the notification to be sent. To query the
-- notification types supported by Amazon EC2 Auto Scaling, call the
-- DescribeAutoScalingNotificationTypes API.
putNotificationConfiguration_notificationTypes :: Lens.Lens' PutNotificationConfiguration [Prelude.Text]
putNotificationConfiguration_notificationTypes = Lens.lens (\PutNotificationConfiguration' {notificationTypes} -> notificationTypes) (\s@PutNotificationConfiguration' {} a -> s {notificationTypes = a} :: PutNotificationConfiguration) Prelude.. Lens.coerced

instance Core.AWSRequest PutNotificationConfiguration where
  type
    AWSResponse PutNotificationConfiguration =
      PutNotificationConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull
      PutNotificationConfigurationResponse'

instance
  Prelude.Hashable
    PutNotificationConfiguration
  where
  hashWithSalt _salt PutNotificationConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` autoScalingGroupName
      `Prelude.hashWithSalt` topicARN
      `Prelude.hashWithSalt` notificationTypes

instance Prelude.NFData PutNotificationConfiguration where
  rnf PutNotificationConfiguration' {..} =
    Prelude.rnf autoScalingGroupName
      `Prelude.seq` Prelude.rnf topicARN
      `Prelude.seq` Prelude.rnf notificationTypes

instance Data.ToHeaders PutNotificationConfiguration where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath PutNotificationConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery PutNotificationConfiguration where
  toQuery PutNotificationConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "PutNotificationConfiguration" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName" Data.=: autoScalingGroupName,
        "TopicARN" Data.=: topicARN,
        "NotificationTypes"
          Data.=: Data.toQueryList "member" notificationTypes
      ]

-- | /See:/ 'newPutNotificationConfigurationResponse' smart constructor.
data PutNotificationConfigurationResponse = PutNotificationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutNotificationConfigurationResponse ::
  PutNotificationConfigurationResponse
newPutNotificationConfigurationResponse =
  PutNotificationConfigurationResponse'

instance
  Prelude.NFData
    PutNotificationConfigurationResponse
  where
  rnf _ = ()
