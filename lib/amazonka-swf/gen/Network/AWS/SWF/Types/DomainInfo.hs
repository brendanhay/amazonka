{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SWF.Types.DomainInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SWF.Types.DomainInfo
  ( DomainInfo (..),

    -- * Smart constructor
    mkDomainInfo,

    -- * Lenses
    diArn,
    diDescription,
    diName,
    diStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SWF.Types.RegistrationStatus

-- | Contains general information about a domain.
--
-- /See:/ 'mkDomainInfo' smart constructor.
data DomainInfo = DomainInfo'
  { arn :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    status :: RegistrationStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainInfo' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the domain.
-- * 'description' - The description of the domain provided through 'RegisterDomain' .
-- * 'name' - The name of the domain. This name is unique within the account.
-- * 'status' - The status of the domain:
--
--
--     * @REGISTERED@ – The domain is properly registered and available. You can use this domain for registering types and creating new workflow executions.
--
--
--     * @DEPRECATED@ – The domain was deprecated using 'DeprecateDomain' , but is still in use. You should not create new workflow executions in this domain.
mkDomainInfo ::
  -- | 'name'
  Lude.Text ->
  -- | 'status'
  RegistrationStatus ->
  DomainInfo
mkDomainInfo pName_ pStatus_ =
  DomainInfo'
    { arn = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_,
      status = pStatus_
    }

-- | The ARN of the domain.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diArn :: Lens.Lens' DomainInfo (Lude.Maybe Lude.Text)
diArn = Lens.lens (arn :: DomainInfo -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: DomainInfo)
{-# DEPRECATED diArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The description of the domain provided through 'RegisterDomain' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DomainInfo (Lude.Maybe Lude.Text)
diDescription = Lens.lens (description :: DomainInfo -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DomainInfo)
{-# DEPRECATED diDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the domain. This name is unique within the account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DomainInfo Lude.Text
diName = Lens.lens (name :: DomainInfo -> Lude.Text) (\s a -> s {name = a} :: DomainInfo)
{-# DEPRECATED diName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The status of the domain:
--
--
--     * @REGISTERED@ – The domain is properly registered and available. You can use this domain for registering types and creating new workflow executions.
--
--
--     * @DEPRECATED@ – The domain was deprecated using 'DeprecateDomain' , but is still in use. You should not create new workflow executions in this domain.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diStatus :: Lens.Lens' DomainInfo RegistrationStatus
diStatus = Lens.lens (status :: DomainInfo -> RegistrationStatus) (\s a -> s {status = a} :: DomainInfo)
{-# DEPRECATED diStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromJSON DomainInfo where
  parseJSON =
    Lude.withObject
      "DomainInfo"
      ( \x ->
          DomainInfo'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "description")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "status")
      )
