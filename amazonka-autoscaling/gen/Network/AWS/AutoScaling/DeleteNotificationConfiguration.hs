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
-- Module      : Network.AWS.AutoScaling.DeleteNotificationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified notification.
module Network.AWS.AutoScaling.DeleteNotificationConfiguration
  ( -- * Creating a Request
    DeleteNotificationConfiguration (..),
    newDeleteNotificationConfiguration,

    -- * Request Lenses
    deleteNotificationConfiguration_autoScalingGroupName,
    deleteNotificationConfiguration_topicARN,

    -- * Destructuring the Response
    DeleteNotificationConfigurationResponse (..),
    newDeleteNotificationConfigurationResponse,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNotificationConfiguration' smart constructor.
data DeleteNotificationConfiguration = DeleteNotificationConfiguration'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
    -- (Amazon SNS) topic.
    topicARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingGroupName', 'deleteNotificationConfiguration_autoScalingGroupName' - The name of the Auto Scaling group.
--
-- 'topicARN', 'deleteNotificationConfiguration_topicARN' - The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (Amazon SNS) topic.
newDeleteNotificationConfiguration ::
  -- | 'autoScalingGroupName'
  Prelude.Text ->
  -- | 'topicARN'
  Prelude.Text ->
  DeleteNotificationConfiguration
newDeleteNotificationConfiguration
  pAutoScalingGroupName_
  pTopicARN_ =
    DeleteNotificationConfiguration'
      { autoScalingGroupName =
          pAutoScalingGroupName_,
        topicARN = pTopicARN_
      }

-- | The name of the Auto Scaling group.
deleteNotificationConfiguration_autoScalingGroupName :: Lens.Lens' DeleteNotificationConfiguration Prelude.Text
deleteNotificationConfiguration_autoScalingGroupName = Lens.lens (\DeleteNotificationConfiguration' {autoScalingGroupName} -> autoScalingGroupName) (\s@DeleteNotificationConfiguration' {} a -> s {autoScalingGroupName = a} :: DeleteNotificationConfiguration)

-- | The Amazon Resource Name (ARN) of the Amazon Simple Notification Service
-- (Amazon SNS) topic.
deleteNotificationConfiguration_topicARN :: Lens.Lens' DeleteNotificationConfiguration Prelude.Text
deleteNotificationConfiguration_topicARN = Lens.lens (\DeleteNotificationConfiguration' {topicARN} -> topicARN) (\s@DeleteNotificationConfiguration' {} a -> s {topicARN = a} :: DeleteNotificationConfiguration)

instance
  Prelude.AWSRequest
    DeleteNotificationConfiguration
  where
  type
    Rs DeleteNotificationConfiguration =
      DeleteNotificationConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DeleteNotificationConfigurationResponse'

instance
  Prelude.Hashable
    DeleteNotificationConfiguration

instance
  Prelude.NFData
    DeleteNotificationConfiguration

instance
  Prelude.ToHeaders
    DeleteNotificationConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    DeleteNotificationConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeleteNotificationConfiguration
  where
  toQuery DeleteNotificationConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "DeleteNotificationConfiguration" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2011-01-01" :: Prelude.ByteString),
        "AutoScalingGroupName"
          Prelude.=: autoScalingGroupName,
        "TopicARN" Prelude.=: topicARN
      ]

-- | /See:/ 'newDeleteNotificationConfigurationResponse' smart constructor.
data DeleteNotificationConfigurationResponse = DeleteNotificationConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteNotificationConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteNotificationConfigurationResponse ::
  DeleteNotificationConfigurationResponse
newDeleteNotificationConfigurationResponse =
  DeleteNotificationConfigurationResponse'

instance
  Prelude.NFData
    DeleteNotificationConfigurationResponse
