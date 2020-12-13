{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.DeleteRealtimeLogConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a real-time log configuration.
--
-- You cannot delete a real-time log configuration if it’s attached to a cache behavior. First update your distributions to remove the real-time log configuration from all cache behaviors, then delete the real-time log configuration.
-- To delete a real-time log configuration, you can provide the configuration’s name or its Amazon Resource Name (ARN). You must provide at least one. If you provide both, CloudFront uses the name to identify the real-time log configuration to delete.
module Network.AWS.CloudFront.DeleteRealtimeLogConfig
  ( -- * Creating a request
    DeleteRealtimeLogConfig (..),
    mkDeleteRealtimeLogConfig,

    -- ** Request lenses
    drlcARN,
    drlcName,

    -- * Destructuring the response
    DeleteRealtimeLogConfigResponse (..),
    mkDeleteRealtimeLogConfigResponse,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteRealtimeLogConfig' smart constructor.
data DeleteRealtimeLogConfig = DeleteRealtimeLogConfig'
  { -- | The Amazon Resource Name (ARN) of the real-time log configuration to delete.
    arn :: Lude.Maybe Lude.Text,
    -- | The name of the real-time log configuration to delete.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRealtimeLogConfig' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the real-time log configuration to delete.
-- * 'name' - The name of the real-time log configuration to delete.
mkDeleteRealtimeLogConfig ::
  DeleteRealtimeLogConfig
mkDeleteRealtimeLogConfig =
  DeleteRealtimeLogConfig' {arn = Lude.Nothing, name = Lude.Nothing}

-- | The Amazon Resource Name (ARN) of the real-time log configuration to delete.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drlcARN :: Lens.Lens' DeleteRealtimeLogConfig (Lude.Maybe Lude.Text)
drlcARN = Lens.lens (arn :: DeleteRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DeleteRealtimeLogConfig)
{-# DEPRECATED drlcARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the real-time log configuration to delete.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drlcName :: Lens.Lens' DeleteRealtimeLogConfig (Lude.Maybe Lude.Text)
drlcName = Lens.lens (name :: DeleteRealtimeLogConfig -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteRealtimeLogConfig)
{-# DEPRECATED drlcName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteRealtimeLogConfig where
  type Rs DeleteRealtimeLogConfig = DeleteRealtimeLogConfigResponse
  request = Req.postXML cloudFrontService
  response = Res.receiveNull DeleteRealtimeLogConfigResponse'

instance Lude.ToElement DeleteRealtimeLogConfig where
  toElement =
    Lude.mkElement
      "{http://cloudfront.amazonaws.com/doc/2020-05-31/}DeleteRealtimeLogConfigRequest"

instance Lude.ToHeaders DeleteRealtimeLogConfig where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteRealtimeLogConfig where
  toPath = Lude.const "/2020-05-31/delete-realtime-log-config/"

instance Lude.ToQuery DeleteRealtimeLogConfig where
  toQuery = Lude.const Lude.mempty

instance Lude.ToXML DeleteRealtimeLogConfig where
  toXML DeleteRealtimeLogConfig' {..} =
    Lude.mconcat ["ARN" Lude.@= arn, "Name" Lude.@= name]

-- | /See:/ 'mkDeleteRealtimeLogConfigResponse' smart constructor.
data DeleteRealtimeLogConfigResponse = DeleteRealtimeLogConfigResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteRealtimeLogConfigResponse' with the minimum fields required to make a request.
mkDeleteRealtimeLogConfigResponse ::
  DeleteRealtimeLogConfigResponse
mkDeleteRealtimeLogConfigResponse =
  DeleteRealtimeLogConfigResponse'
