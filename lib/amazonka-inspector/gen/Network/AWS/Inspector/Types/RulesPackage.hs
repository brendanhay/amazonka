{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.RulesPackage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.RulesPackage
  ( RulesPackage (..),

    -- * Smart constructor
    mkRulesPackage,

    -- * Lenses
    rpDescription,
    rpArn,
    rpName,
    rpVersion,
    rpProvider,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about an Amazon Inspector rules package. This data type is used as the response element in the 'DescribeRulesPackages' action.
--
-- /See:/ 'mkRulesPackage' smart constructor.
data RulesPackage = RulesPackage'
  { description ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Text,
    name :: Lude.Text,
    version :: Lude.Text,
    provider :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RulesPackage' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the rules package.
-- * 'description' - The description of the rules package.
-- * 'name' - The name of the rules package.
-- * 'provider' - The provider of the rules package.
-- * 'version' - The version ID of the rules package.
mkRulesPackage ::
  -- | 'arn'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'version'
  Lude.Text ->
  -- | 'provider'
  Lude.Text ->
  RulesPackage
mkRulesPackage pArn_ pName_ pVersion_ pProvider_ =
  RulesPackage'
    { description = Lude.Nothing,
      arn = pArn_,
      name = pName_,
      version = pVersion_,
      provider = pProvider_
    }

-- | The description of the rules package.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpDescription :: Lens.Lens' RulesPackage (Lude.Maybe Lude.Text)
rpDescription = Lens.lens (description :: RulesPackage -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: RulesPackage)
{-# DEPRECATED rpDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The ARN of the rules package.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpArn :: Lens.Lens' RulesPackage Lude.Text
rpArn = Lens.lens (arn :: RulesPackage -> Lude.Text) (\s a -> s {arn = a} :: RulesPackage)
{-# DEPRECATED rpArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the rules package.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpName :: Lens.Lens' RulesPackage Lude.Text
rpName = Lens.lens (name :: RulesPackage -> Lude.Text) (\s a -> s {name = a} :: RulesPackage)
{-# DEPRECATED rpName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The version ID of the rules package.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpVersion :: Lens.Lens' RulesPackage Lude.Text
rpVersion = Lens.lens (version :: RulesPackage -> Lude.Text) (\s a -> s {version = a} :: RulesPackage)
{-# DEPRECATED rpVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The provider of the rules package.
--
-- /Note:/ Consider using 'provider' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpProvider :: Lens.Lens' RulesPackage Lude.Text
rpProvider = Lens.lens (provider :: RulesPackage -> Lude.Text) (\s a -> s {provider = a} :: RulesPackage)
{-# DEPRECATED rpProvider "Use generic-lens or generic-optics with 'provider' instead." #-}

instance Lude.FromJSON RulesPackage where
  parseJSON =
    Lude.withObject
      "RulesPackage"
      ( \x ->
          RulesPackage'
            Lude.<$> (x Lude..:? "description")
            Lude.<*> (x Lude..: "arn")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "version")
            Lude.<*> (x Lude..: "provider")
      )
