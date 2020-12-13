{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.CORSConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.CORSConfiguration
  ( CORSConfiguration (..),

    -- * Smart constructor
    mkCORSConfiguration,

    -- * Lenses
    ccCORSRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.CORSRule

-- | Describes the cross-origin access configuration for objects in an Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /See:/ 'mkCORSConfiguration' smart constructor.
newtype CORSConfiguration = CORSConfiguration'
  { -- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
    corsRules :: [CORSRule]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CORSConfiguration' with the minimum fields required to make a request.
--
-- * 'corsRules' - A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
mkCORSConfiguration ::
  CORSConfiguration
mkCORSConfiguration = CORSConfiguration' {corsRules = Lude.mempty}

-- | A set of origins and methods (cross-origin access that you want to allow). You can add up to 100 rules to the configuration.
--
-- /Note:/ Consider using 'corsRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCORSRules :: Lens.Lens' CORSConfiguration [CORSRule]
ccCORSRules = Lens.lens (corsRules :: CORSConfiguration -> [CORSRule]) (\s a -> s {corsRules = a} :: CORSConfiguration)
{-# DEPRECATED ccCORSRules "Use generic-lens or generic-optics with 'corsRules' instead." #-}

instance Lude.ToXML CORSConfiguration where
  toXML CORSConfiguration' {..} =
    Lude.mconcat [Lude.toXMLList "CORSRule" corsRules]
