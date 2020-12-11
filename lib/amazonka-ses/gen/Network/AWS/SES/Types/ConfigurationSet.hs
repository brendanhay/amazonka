-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.ConfigurationSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.ConfigurationSet
  ( ConfigurationSet (..),

    -- * Smart constructor
    mkConfigurationSet,

    -- * Lenses
    csName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The name of the configuration set.
--
-- Configuration sets let you create groups of rules that you can apply to the emails you send using Amazon SES. For more information about using configuration sets, see <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/using-configuration-sets.html Using Amazon SES Configuration Sets> in the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/ Amazon SES Developer Guide> .
--
-- /See:/ 'mkConfigurationSet' smart constructor.
newtype ConfigurationSet = ConfigurationSet' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ConfigurationSet' with the minimum fields required to make a request.
--
-- * 'name' - The name of the configuration set. The name must meet the following requirements:
--
--
--     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain 64 characters or fewer.
mkConfigurationSet ::
  -- | 'name'
  Lude.Text ->
  ConfigurationSet
mkConfigurationSet pName_ = ConfigurationSet' {name = pName_}

-- | The name of the configuration set. The name must meet the following requirements:
--
--
--     * Contain only letters (a-z, A-Z), numbers (0-9), underscores (_), or dashes (-).
--
--
--     * Contain 64 characters or fewer.
--
--
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csName :: Lens.Lens' ConfigurationSet Lude.Text
csName = Lens.lens (name :: ConfigurationSet -> Lude.Text) (\s a -> s {name = a} :: ConfigurationSet)
{-# DEPRECATED csName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML ConfigurationSet where
  parseXML x = ConfigurationSet' Lude.<$> (x Lude..@ "Name")

instance Lude.ToQuery ConfigurationSet where
  toQuery ConfigurationSet' {..} = Lude.mconcat ["Name" Lude.=: name]
