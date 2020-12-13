{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.GetLayerVersionByARN
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a version of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> , with a link to download the layer archive that's valid for 10 minutes.
module Network.AWS.Lambda.GetLayerVersionByARN
  ( -- * Creating a request
    GetLayerVersionByARN (..),
    mkGetLayerVersionByARN,

    -- ** Request lenses
    glvbaARN,

    -- * Destructuring the response
    GetLayerVersionResponse (..),
    mkGetLayerVersionResponse,

    -- ** Response lenses
    glvLayerVersionARN,
    glvContent,
    glvCreatedDate,
    glvVersion,
    glvLicenseInfo,
    glvLayerARN,
    glvDescription,
    glvCompatibleRuntimes,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLayerVersionByARN' smart constructor.
newtype GetLayerVersionByARN = GetLayerVersionByARN'
  { -- | The ARN of the layer version.
    arn :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLayerVersionByARN' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the layer version.
mkGetLayerVersionByARN ::
  -- | 'arn'
  Lude.Text ->
  GetLayerVersionByARN
mkGetLayerVersionByARN pARN_ = GetLayerVersionByARN' {arn = pARN_}

-- | The ARN of the layer version.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glvbaARN :: Lens.Lens' GetLayerVersionByARN Lude.Text
glvbaARN = Lens.lens (arn :: GetLayerVersionByARN -> Lude.Text) (\s a -> s {arn = a} :: GetLayerVersionByARN)
{-# DEPRECATED glvbaARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.AWSRequest GetLayerVersionByARN where
  type Rs GetLayerVersionByARN = GetLayerVersionResponse
  request = Req.get lambdaService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders GetLayerVersionByARN where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetLayerVersionByARN where
  toPath = Lude.const "/2018-10-31/layers"

instance Lude.ToQuery GetLayerVersionByARN where
  toQuery GetLayerVersionByARN' {..} =
    Lude.mconcat ["Arn" Lude.=: arn, "find=LayerVersion"]
