{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.PutAccountSettingDefault
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an account setting for all IAM users on an account for whom no individual account setting has been specified. Account settings are set on a per-Region basis.
module Network.AWS.ECS.PutAccountSettingDefault
  ( -- * Creating a request
    PutAccountSettingDefault (..),
    mkPutAccountSettingDefault,

    -- ** Request lenses
    pasdValue,
    pasdName,

    -- * Destructuring the response
    PutAccountSettingDefaultResponse (..),
    mkPutAccountSettingDefaultResponse,

    -- ** Response lenses
    pasdrsSetting,
    pasdrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutAccountSettingDefault' smart constructor.
data PutAccountSettingDefault = PutAccountSettingDefault'
  { -- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
    value :: Lude.Text,
    -- | The resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
    name :: SettingName
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAccountSettingDefault' with the minimum fields required to make a request.
--
-- * 'value' - The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
-- * 'name' - The resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
mkPutAccountSettingDefault ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  SettingName ->
  PutAccountSettingDefault
mkPutAccountSettingDefault pValue_ pName_ =
  PutAccountSettingDefault' {value = pValue_, name = pName_}

-- | The account setting value for the specified principal ARN. Accepted values are @enabled@ and @disabled@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdValue :: Lens.Lens' PutAccountSettingDefault Lude.Text
pasdValue = Lens.lens (value :: PutAccountSettingDefault -> Lude.Text) (\s a -> s {value = a} :: PutAccountSettingDefault)
{-# DEPRECATED pasdValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The resource name for which to modify the account setting. If @serviceLongArnFormat@ is specified, the ARN for your Amazon ECS services is affected. If @taskLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS tasks is affected. If @containerInstanceLongArnFormat@ is specified, the ARN and resource ID for your Amazon ECS container instances is affected. If @awsvpcTrunking@ is specified, the ENI limit for your Amazon ECS container instances is affected. If @containerInsights@ is specified, the default setting for CloudWatch Container Insights for your clusters is affected.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdName :: Lens.Lens' PutAccountSettingDefault SettingName
pasdName = Lens.lens (name :: PutAccountSettingDefault -> SettingName) (\s a -> s {name = a} :: PutAccountSettingDefault)
{-# DEPRECATED pasdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest PutAccountSettingDefault where
  type Rs PutAccountSettingDefault = PutAccountSettingDefaultResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          PutAccountSettingDefaultResponse'
            Lude.<$> (x Lude..?> "setting") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PutAccountSettingDefault where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.PutAccountSettingDefault" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PutAccountSettingDefault where
  toJSON PutAccountSettingDefault' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("value" Lude..= value),
            Lude.Just ("name" Lude..= name)
          ]
      )

instance Lude.ToPath PutAccountSettingDefault where
  toPath = Lude.const "/"

instance Lude.ToQuery PutAccountSettingDefault where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutAccountSettingDefaultResponse' smart constructor.
data PutAccountSettingDefaultResponse = PutAccountSettingDefaultResponse'
  { setting :: Lude.Maybe Setting,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutAccountSettingDefaultResponse' with the minimum fields required to make a request.
--
-- * 'setting' -
-- * 'responseStatus' - The response status code.
mkPutAccountSettingDefaultResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PutAccountSettingDefaultResponse
mkPutAccountSettingDefaultResponse pResponseStatus_ =
  PutAccountSettingDefaultResponse'
    { setting = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'setting' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdrsSetting :: Lens.Lens' PutAccountSettingDefaultResponse (Lude.Maybe Setting)
pasdrsSetting = Lens.lens (setting :: PutAccountSettingDefaultResponse -> Lude.Maybe Setting) (\s a -> s {setting = a} :: PutAccountSettingDefaultResponse)
{-# DEPRECATED pasdrsSetting "Use generic-lens or generic-optics with 'setting' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pasdrsResponseStatus :: Lens.Lens' PutAccountSettingDefaultResponse Lude.Int
pasdrsResponseStatus = Lens.lens (responseStatus :: PutAccountSettingDefaultResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PutAccountSettingDefaultResponse)
{-# DEPRECATED pasdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
