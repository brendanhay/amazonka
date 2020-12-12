{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.AliasConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lambda.Types.AliasConfiguration
  ( AliasConfiguration (..),

    -- * Smart constructor
    mkAliasConfiguration,

    -- * Lenses
    acRoutingConfig,
    acName,
    acFunctionVersion,
    acAliasARN,
    acDescription,
    acRevisionId,
  )
where

import Network.AWS.Lambda.Types.AliasRoutingConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides configuration information about a Lambda function <https://docs.aws.amazon.com/lambda/latest/dg/versioning-aliases.html alias> .
--
-- /See:/ 'mkAliasConfiguration' smart constructor.
data AliasConfiguration = AliasConfiguration'
  { routingConfig ::
      Lude.Maybe AliasRoutingConfiguration,
    name :: Lude.Maybe Lude.Text,
    functionVersion :: Lude.Maybe Lude.Text,
    aliasARN :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    revisionId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AliasConfiguration' with the minimum fields required to make a request.
--
-- * 'aliasARN' - The Amazon Resource Name (ARN) of the alias.
-- * 'description' - A description of the alias.
-- * 'functionVersion' - The function version that the alias invokes.
-- * 'name' - The name of the alias.
-- * 'revisionId' - A unique identifier that changes when you update the alias.
-- * 'routingConfig' - The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
mkAliasConfiguration ::
  AliasConfiguration
mkAliasConfiguration =
  AliasConfiguration'
    { routingConfig = Lude.Nothing,
      name = Lude.Nothing,
      functionVersion = Lude.Nothing,
      aliasARN = Lude.Nothing,
      description = Lude.Nothing,
      revisionId = Lude.Nothing
    }

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/lambda-traffic-shifting-using-aliases.html routing configuration> of the alias.
--
-- /Note:/ Consider using 'routingConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acRoutingConfig :: Lens.Lens' AliasConfiguration (Lude.Maybe AliasRoutingConfiguration)
acRoutingConfig = Lens.lens (routingConfig :: AliasConfiguration -> Lude.Maybe AliasRoutingConfiguration) (\s a -> s {routingConfig = a} :: AliasConfiguration)
{-# DEPRECATED acRoutingConfig "Use generic-lens or generic-optics with 'routingConfig' instead." #-}

-- | The name of the alias.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acName :: Lens.Lens' AliasConfiguration (Lude.Maybe Lude.Text)
acName = Lens.lens (name :: AliasConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: AliasConfiguration)
{-# DEPRECATED acName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The function version that the alias invokes.
--
-- /Note:/ Consider using 'functionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acFunctionVersion :: Lens.Lens' AliasConfiguration (Lude.Maybe Lude.Text)
acFunctionVersion = Lens.lens (functionVersion :: AliasConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {functionVersion = a} :: AliasConfiguration)
{-# DEPRECATED acFunctionVersion "Use generic-lens or generic-optics with 'functionVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the alias.
--
-- /Note:/ Consider using 'aliasARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acAliasARN :: Lens.Lens' AliasConfiguration (Lude.Maybe Lude.Text)
acAliasARN = Lens.lens (aliasARN :: AliasConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {aliasARN = a} :: AliasConfiguration)
{-# DEPRECATED acAliasARN "Use generic-lens or generic-optics with 'aliasARN' instead." #-}

-- | A description of the alias.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acDescription :: Lens.Lens' AliasConfiguration (Lude.Maybe Lude.Text)
acDescription = Lens.lens (description :: AliasConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: AliasConfiguration)
{-# DEPRECATED acDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A unique identifier that changes when you update the alias.
--
-- /Note:/ Consider using 'revisionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
acRevisionId :: Lens.Lens' AliasConfiguration (Lude.Maybe Lude.Text)
acRevisionId = Lens.lens (revisionId :: AliasConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {revisionId = a} :: AliasConfiguration)
{-# DEPRECATED acRevisionId "Use generic-lens or generic-optics with 'revisionId' instead." #-}

instance Lude.FromJSON AliasConfiguration where
  parseJSON =
    Lude.withObject
      "AliasConfiguration"
      ( \x ->
          AliasConfiguration'
            Lude.<$> (x Lude..:? "RoutingConfig")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "FunctionVersion")
            Lude.<*> (x Lude..:? "AliasArn")
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..:? "RevisionId")
      )
