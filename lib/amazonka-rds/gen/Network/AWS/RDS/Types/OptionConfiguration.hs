-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.OptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.OptionConfiguration
  ( OptionConfiguration (..),

    -- * Smart constructor
    mkOptionConfiguration,

    -- * Lenses
    ocOptionSettings,
    ocVPCSecurityGroupMemberships,
    ocDBSecurityGroupMemberships,
    ocOptionVersion,
    ocPort,
    ocOptionName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types.OptionSetting

-- | A list of all available options
--
-- /See:/ 'mkOptionConfiguration' smart constructor.
data OptionConfiguration = OptionConfiguration'
  { optionSettings ::
      Lude.Maybe [OptionSetting],
    vpcSecurityGroupMemberships ::
      Lude.Maybe [Lude.Text],
    dbSecurityGroupMemberships ::
      Lude.Maybe [Lude.Text],
    optionVersion :: Lude.Maybe Lude.Text,
    port :: Lude.Maybe Lude.Int,
    optionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OptionConfiguration' with the minimum fields required to make a request.
--
-- * 'dbSecurityGroupMemberships' - A list of DBSecurityGroupMembership name strings used for this option.
-- * 'optionName' - The configuration of options to include in a group.
-- * 'optionSettings' - The option settings to include in an option group.
-- * 'optionVersion' - The version for the option.
-- * 'port' - The optional port for the option.
-- * 'vpcSecurityGroupMemberships' - A list of VpcSecurityGroupMembership name strings used for this option.
mkOptionConfiguration ::
  -- | 'optionName'
  Lude.Text ->
  OptionConfiguration
mkOptionConfiguration pOptionName_ =
  OptionConfiguration'
    { optionSettings = Lude.Nothing,
      vpcSecurityGroupMemberships = Lude.Nothing,
      dbSecurityGroupMemberships = Lude.Nothing,
      optionVersion = Lude.Nothing,
      port = Lude.Nothing,
      optionName = pOptionName_
    }

-- | The option settings to include in an option group.
--
-- /Note:/ Consider using 'optionSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionSettings :: Lens.Lens' OptionConfiguration (Lude.Maybe [OptionSetting])
ocOptionSettings = Lens.lens (optionSettings :: OptionConfiguration -> Lude.Maybe [OptionSetting]) (\s a -> s {optionSettings = a} :: OptionConfiguration)
{-# DEPRECATED ocOptionSettings "Use generic-lens or generic-optics with 'optionSettings' instead." #-}

-- | A list of VpcSecurityGroupMembership name strings used for this option.
--
-- /Note:/ Consider using 'vpcSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocVPCSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Lude.Maybe [Lude.Text])
ocVPCSecurityGroupMemberships = Lens.lens (vpcSecurityGroupMemberships :: OptionConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {vpcSecurityGroupMemberships = a} :: OptionConfiguration)
{-# DEPRECATED ocVPCSecurityGroupMemberships "Use generic-lens or generic-optics with 'vpcSecurityGroupMemberships' instead." #-}

-- | A list of DBSecurityGroupMembership name strings used for this option.
--
-- /Note:/ Consider using 'dbSecurityGroupMemberships' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocDBSecurityGroupMemberships :: Lens.Lens' OptionConfiguration (Lude.Maybe [Lude.Text])
ocDBSecurityGroupMemberships = Lens.lens (dbSecurityGroupMemberships :: OptionConfiguration -> Lude.Maybe [Lude.Text]) (\s a -> s {dbSecurityGroupMemberships = a} :: OptionConfiguration)
{-# DEPRECATED ocDBSecurityGroupMemberships "Use generic-lens or generic-optics with 'dbSecurityGroupMemberships' instead." #-}

-- | The version for the option.
--
-- /Note:/ Consider using 'optionVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionVersion :: Lens.Lens' OptionConfiguration (Lude.Maybe Lude.Text)
ocOptionVersion = Lens.lens (optionVersion :: OptionConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {optionVersion = a} :: OptionConfiguration)
{-# DEPRECATED ocOptionVersion "Use generic-lens or generic-optics with 'optionVersion' instead." #-}

-- | The optional port for the option.
--
-- /Note:/ Consider using 'port' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocPort :: Lens.Lens' OptionConfiguration (Lude.Maybe Lude.Int)
ocPort = Lens.lens (port :: OptionConfiguration -> Lude.Maybe Lude.Int) (\s a -> s {port = a} :: OptionConfiguration)
{-# DEPRECATED ocPort "Use generic-lens or generic-optics with 'port' instead." #-}

-- | The configuration of options to include in a group.
--
-- /Note:/ Consider using 'optionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocOptionName :: Lens.Lens' OptionConfiguration Lude.Text
ocOptionName = Lens.lens (optionName :: OptionConfiguration -> Lude.Text) (\s a -> s {optionName = a} :: OptionConfiguration)
{-# DEPRECATED ocOptionName "Use generic-lens or generic-optics with 'optionName' instead." #-}

instance Lude.ToQuery OptionConfiguration where
  toQuery OptionConfiguration' {..} =
    Lude.mconcat
      [ "OptionSettings"
          Lude.=: Lude.toQuery
            (Lude.toQueryList "OptionSetting" Lude.<$> optionSettings),
        "VpcSecurityGroupMemberships"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "VpcSecurityGroupId"
                Lude.<$> vpcSecurityGroupMemberships
            ),
        "DBSecurityGroupMemberships"
          Lude.=: Lude.toQuery
            ( Lude.toQueryList "DBSecurityGroupName"
                Lude.<$> dbSecurityGroupMemberships
            ),
        "OptionVersion" Lude.=: optionVersion,
        "Port" Lude.=: port,
        "OptionName" Lude.=: optionName
      ]
