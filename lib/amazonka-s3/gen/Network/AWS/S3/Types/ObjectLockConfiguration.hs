-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.ObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.ObjectLockConfiguration
  ( ObjectLockConfiguration (..),

    -- * Smart constructor
    mkObjectLockConfiguration,

    -- * Lenses
    olcObjectLockEnabled,
    olcRule,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.ObjectLockEnabled
import Network.AWS.S3.Types.ObjectLockRule

-- | The container element for Object Lock configuration parameters.
--
-- /See:/ 'mkObjectLockConfiguration' smart constructor.
data ObjectLockConfiguration = ObjectLockConfiguration'
  { objectLockEnabled ::
      Lude.Maybe ObjectLockEnabled,
    rule :: Lude.Maybe ObjectLockRule
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ObjectLockConfiguration' with the minimum fields required to make a request.
--
-- * 'objectLockEnabled' - Indicates whether this bucket has an Object Lock configuration enabled.
-- * 'rule' - The Object Lock rule in place for the specified object.
mkObjectLockConfiguration ::
  ObjectLockConfiguration
mkObjectLockConfiguration =
  ObjectLockConfiguration'
    { objectLockEnabled = Lude.Nothing,
      rule = Lude.Nothing
    }

-- | Indicates whether this bucket has an Object Lock configuration enabled.
--
-- /Note:/ Consider using 'objectLockEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olcObjectLockEnabled :: Lens.Lens' ObjectLockConfiguration (Lude.Maybe ObjectLockEnabled)
olcObjectLockEnabled = Lens.lens (objectLockEnabled :: ObjectLockConfiguration -> Lude.Maybe ObjectLockEnabled) (\s a -> s {objectLockEnabled = a} :: ObjectLockConfiguration)
{-# DEPRECATED olcObjectLockEnabled "Use generic-lens or generic-optics with 'objectLockEnabled' instead." #-}

-- | The Object Lock rule in place for the specified object.
--
-- /Note:/ Consider using 'rule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
olcRule :: Lens.Lens' ObjectLockConfiguration (Lude.Maybe ObjectLockRule)
olcRule = Lens.lens (rule :: ObjectLockConfiguration -> Lude.Maybe ObjectLockRule) (\s a -> s {rule = a} :: ObjectLockConfiguration)
{-# DEPRECATED olcRule "Use generic-lens or generic-optics with 'rule' instead." #-}

instance Lude.FromXML ObjectLockConfiguration where
  parseXML x =
    ObjectLockConfiguration'
      Lude.<$> (x Lude..@? "ObjectLockEnabled") Lude.<*> (x Lude..@? "Rule")

instance Lude.ToXML ObjectLockConfiguration where
  toXML ObjectLockConfiguration' {..} =
    Lude.mconcat
      [ "ObjectLockEnabled" Lude.@= objectLockEnabled,
        "Rule" Lude.@= rule
      ]
