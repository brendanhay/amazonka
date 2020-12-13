{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.UpdateElasticIP
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a registered Elastic IP address's name. For more information, see <https://docs.aws.amazon.com/opsworks/latest/userguide/resources.html Resource Management> .
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
module Network.AWS.OpsWorks.UpdateElasticIP
  ( -- * Creating a request
    UpdateElasticIP (..),
    mkUpdateElasticIP,

    -- ** Request lenses
    ueiElasticIP,
    ueiName,

    -- * Destructuring the response
    UpdateElasticIPResponse (..),
    mkUpdateElasticIPResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateElasticIP' smart constructor.
data UpdateElasticIP = UpdateElasticIP'
  { -- | The IP address for which you want to update the name.
    elasticIP :: Lude.Text,
    -- | The new name.
    name :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateElasticIP' with the minimum fields required to make a request.
--
-- * 'elasticIP' - The IP address for which you want to update the name.
-- * 'name' - The new name.
mkUpdateElasticIP ::
  -- | 'elasticIP'
  Lude.Text ->
  UpdateElasticIP
mkUpdateElasticIP pElasticIP_ =
  UpdateElasticIP' {elasticIP = pElasticIP_, name = Lude.Nothing}

-- | The IP address for which you want to update the name.
--
-- /Note:/ Consider using 'elasticIP' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueiElasticIP :: Lens.Lens' UpdateElasticIP Lude.Text
ueiElasticIP = Lens.lens (elasticIP :: UpdateElasticIP -> Lude.Text) (\s a -> s {elasticIP = a} :: UpdateElasticIP)
{-# DEPRECATED ueiElasticIP "Use generic-lens or generic-optics with 'elasticIP' instead." #-}

-- | The new name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ueiName :: Lens.Lens' UpdateElasticIP (Lude.Maybe Lude.Text)
ueiName = Lens.lens (name :: UpdateElasticIP -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateElasticIP)
{-# DEPRECATED ueiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest UpdateElasticIP where
  type Rs UpdateElasticIP = UpdateElasticIPResponse
  request = Req.postJSON opsWorksService
  response = Res.receiveNull UpdateElasticIPResponse'

instance Lude.ToHeaders UpdateElasticIP where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OpsWorks_20130218.UpdateElasticIp" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateElasticIP where
  toJSON UpdateElasticIP' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ElasticIp" Lude..= elasticIP),
            ("Name" Lude..=) Lude.<$> name
          ]
      )

instance Lude.ToPath UpdateElasticIP where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateElasticIP where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateElasticIPResponse' smart constructor.
data UpdateElasticIPResponse = UpdateElasticIPResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateElasticIPResponse' with the minimum fields required to make a request.
mkUpdateElasticIPResponse ::
  UpdateElasticIPResponse
mkUpdateElasticIPResponse = UpdateElasticIPResponse'
