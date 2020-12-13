{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationPolicyInfo
  ( AutomaticTapeCreationPolicyInfo (..),

    -- * Smart constructor
    mkAutomaticTapeCreationPolicyInfo,

    -- * Lenses
    atcpiGatewayARN,
    atcpiAutomaticTapeCreationRules,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule

-- | Information about the gateway's automatic tape creation policies, including the automatic tape creation rules and the gateway that is using the policies.
--
-- /See:/ 'mkAutomaticTapeCreationPolicyInfo' smart constructor.
data AutomaticTapeCreationPolicyInfo = AutomaticTapeCreationPolicyInfo'
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
    automaticTapeCreationRules :: Lude.Maybe (Lude.NonEmpty AutomaticTapeCreationRule)
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomaticTapeCreationPolicyInfo' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'automaticTapeCreationRules' - An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
mkAutomaticTapeCreationPolicyInfo ::
  AutomaticTapeCreationPolicyInfo
mkAutomaticTapeCreationPolicyInfo =
  AutomaticTapeCreationPolicyInfo'
    { gatewayARN = Lude.Nothing,
      automaticTapeCreationRules = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcpiGatewayARN :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Lude.Maybe Lude.Text)
atcpiGatewayARN = Lens.lens (gatewayARN :: AutomaticTapeCreationPolicyInfo -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: AutomaticTapeCreationPolicyInfo)
{-# DEPRECATED atcpiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An automatic tape creation policy consists of a list of automatic tape creation rules. This returns the rules that determine when and how to automatically create new tapes.
--
-- /Note:/ Consider using 'automaticTapeCreationRules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcpiAutomaticTapeCreationRules :: Lens.Lens' AutomaticTapeCreationPolicyInfo (Lude.Maybe (Lude.NonEmpty AutomaticTapeCreationRule))
atcpiAutomaticTapeCreationRules = Lens.lens (automaticTapeCreationRules :: AutomaticTapeCreationPolicyInfo -> Lude.Maybe (Lude.NonEmpty AutomaticTapeCreationRule)) (\s a -> s {automaticTapeCreationRules = a} :: AutomaticTapeCreationPolicyInfo)
{-# DEPRECATED atcpiAutomaticTapeCreationRules "Use generic-lens or generic-optics with 'automaticTapeCreationRules' instead." #-}

instance Lude.FromJSON AutomaticTapeCreationPolicyInfo where
  parseJSON =
    Lude.withObject
      "AutomaticTapeCreationPolicyInfo"
      ( \x ->
          AutomaticTapeCreationPolicyInfo'
            Lude.<$> (x Lude..:? "GatewayARN")
            Lude.<*> (x Lude..:? "AutomaticTapeCreationRules")
      )
