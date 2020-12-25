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
    diName,
    diStatus,
    diArn,
    diDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SWF.Types.Arn as Types
import qualified Network.AWS.SWF.Types.Description as Types
import qualified Network.AWS.SWF.Types.DomainName as Types
import qualified Network.AWS.SWF.Types.RegistrationStatus as Types

-- | Contains general information about a domain.
--
-- /See:/ 'mkDomainInfo' smart constructor.
data DomainInfo = DomainInfo'
  { -- | The name of the domain. This name is unique within the account.
    name :: Types.DomainName,
    -- | The status of the domain:
    --
    --
    --     * @REGISTERED@ – The domain is properly registered and available. You can use this domain for registering types and creating new workflow executions.
    --
    --
    --     * @DEPRECATED@ – The domain was deprecated using 'DeprecateDomain' , but is still in use. You should not create new workflow executions in this domain.
    status :: Types.RegistrationStatus,
    -- | The ARN of the domain.
    arn :: Core.Maybe Types.Arn,
    -- | The description of the domain provided through 'RegisterDomain' .
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DomainInfo' value with any optional fields omitted.
mkDomainInfo ::
  -- | 'name'
  Types.DomainName ->
  -- | 'status'
  Types.RegistrationStatus ->
  DomainInfo
mkDomainInfo name status =
  DomainInfo'
    { name,
      status,
      arn = Core.Nothing,
      description = Core.Nothing
    }

-- | The name of the domain. This name is unique within the account.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diName :: Lens.Lens' DomainInfo Types.DomainName
diName = Lens.field @"name"
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
diStatus :: Lens.Lens' DomainInfo Types.RegistrationStatus
diStatus = Lens.field @"status"
{-# DEPRECATED diStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the domain.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diArn :: Lens.Lens' DomainInfo (Core.Maybe Types.Arn)
diArn = Lens.field @"arn"
{-# DEPRECATED diArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The description of the domain provided through 'RegisterDomain' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDescription :: Lens.Lens' DomainInfo (Core.Maybe Types.Description)
diDescription = Lens.field @"description"
{-# DEPRECATED diDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromJSON DomainInfo where
  parseJSON =
    Core.withObject "DomainInfo" Core.$
      \x ->
        DomainInfo'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "status")
          Core.<*> (x Core..:? "arn")
          Core.<*> (x Core..:? "description")
