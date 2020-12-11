-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.BasePathMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.BasePathMapping
  ( BasePathMapping (..),

    -- * Smart constructor
    mkBasePathMapping,

    -- * Lenses
    bpmStage,
    bpmBasePath,
    bpmRestAPIId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the base path that callers of the API must provide as part of the URL after the domain name.
--
-- A custom domain name plus a @BasePathMapping@ specification identifies a deployed 'RestApi' in a given stage of the owner 'Account' .<https://docs.aws.amazon.com/apigateway/latest/developerguide/how-to-custom-domains.html Use Custom Domain Names>
--
-- /See:/ 'mkBasePathMapping' smart constructor.
data BasePathMapping = BasePathMapping'
  { stage ::
      Lude.Maybe Lude.Text,
    basePath :: Lude.Maybe Lude.Text,
    restAPIId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BasePathMapping' with the minimum fields required to make a request.
--
-- * 'basePath' - The base path name that callers of the API must provide as part of the URL after the domain name.
-- * 'restAPIId' - The string identifier of the associated 'RestApi' .
-- * 'stage' - The name of the associated stage.
mkBasePathMapping ::
  BasePathMapping
mkBasePathMapping =
  BasePathMapping'
    { stage = Lude.Nothing,
      basePath = Lude.Nothing,
      restAPIId = Lude.Nothing
    }

-- | The name of the associated stage.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmStage :: Lens.Lens' BasePathMapping (Lude.Maybe Lude.Text)
bpmStage = Lens.lens (stage :: BasePathMapping -> Lude.Maybe Lude.Text) (\s a -> s {stage = a} :: BasePathMapping)
{-# DEPRECATED bpmStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | The base path name that callers of the API must provide as part of the URL after the domain name.
--
-- /Note:/ Consider using 'basePath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmBasePath :: Lens.Lens' BasePathMapping (Lude.Maybe Lude.Text)
bpmBasePath = Lens.lens (basePath :: BasePathMapping -> Lude.Maybe Lude.Text) (\s a -> s {basePath = a} :: BasePathMapping)
{-# DEPRECATED bpmBasePath "Use generic-lens or generic-optics with 'basePath' instead." #-}

-- | The string identifier of the associated 'RestApi' .
--
-- /Note:/ Consider using 'restAPIId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpmRestAPIId :: Lens.Lens' BasePathMapping (Lude.Maybe Lude.Text)
bpmRestAPIId = Lens.lens (restAPIId :: BasePathMapping -> Lude.Maybe Lude.Text) (\s a -> s {restAPIId = a} :: BasePathMapping)
{-# DEPRECATED bpmRestAPIId "Use generic-lens or generic-optics with 'restAPIId' instead." #-}

instance Lude.FromJSON BasePathMapping where
  parseJSON =
    Lude.withObject
      "BasePathMapping"
      ( \x ->
          BasePathMapping'
            Lude.<$> (x Lude..:? "stage")
            Lude.<*> (x Lude..:? "basePath")
            Lude.<*> (x Lude..:? "restApiId")
      )
