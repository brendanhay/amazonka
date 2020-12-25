{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.ContactFlowSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.ContactFlowSummary
  ( ContactFlowSummary (..),

    -- * Smart constructor
    mkContactFlowSummary,

    -- * Lenses
    cfsArn,
    cfsContactFlowType,
    cfsId,
    cfsName,
  )
where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.ContactFlowId as Types
import qualified Network.AWS.Connect.Types.ContactFlowName as Types
import qualified Network.AWS.Connect.Types.ContactFlowType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains summary information about a contact flow.
--
-- You can also create and update contact flows using the <https://docs.aws.amazon.com/connect/latest/adminguide/flow-language.html Amazon Connect Flow language> .
--
-- /See:/ 'mkContactFlowSummary' smart constructor.
data ContactFlowSummary = ContactFlowSummary'
  { -- | The Amazon Resource Name (ARN) of the contact flow.
    arn :: Core.Maybe Types.ARN,
    -- | The type of contact flow.
    contactFlowType :: Core.Maybe Types.ContactFlowType,
    -- | The identifier of the contact flow.
    id :: Core.Maybe Types.ContactFlowId,
    -- | The name of the contact flow.
    name :: Core.Maybe Types.ContactFlowName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ContactFlowSummary' value with any optional fields omitted.
mkContactFlowSummary ::
  ContactFlowSummary
mkContactFlowSummary =
  ContactFlowSummary'
    { arn = Core.Nothing,
      contactFlowType = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the contact flow.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsArn :: Lens.Lens' ContactFlowSummary (Core.Maybe Types.ARN)
cfsArn = Lens.field @"arn"
{-# DEPRECATED cfsArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type of contact flow.
--
-- /Note:/ Consider using 'contactFlowType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsContactFlowType :: Lens.Lens' ContactFlowSummary (Core.Maybe Types.ContactFlowType)
cfsContactFlowType = Lens.field @"contactFlowType"
{-# DEPRECATED cfsContactFlowType "Use generic-lens or generic-optics with 'contactFlowType' instead." #-}

-- | The identifier of the contact flow.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsId :: Lens.Lens' ContactFlowSummary (Core.Maybe Types.ContactFlowId)
cfsId = Lens.field @"id"
{-# DEPRECATED cfsId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the contact flow.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cfsName :: Lens.Lens' ContactFlowSummary (Core.Maybe Types.ContactFlowName)
cfsName = Lens.field @"name"
{-# DEPRECATED cfsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON ContactFlowSummary where
  parseJSON =
    Core.withObject "ContactFlowSummary" Core.$
      \x ->
        ContactFlowSummary'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "ContactFlowType")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
