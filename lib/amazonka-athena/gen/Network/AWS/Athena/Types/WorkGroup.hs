{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.WorkGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.WorkGroup
  ( WorkGroup (..),

    -- * Smart constructor
    mkWorkGroup,

    -- * Lenses
    wgName,
    wgConfiguration,
    wgCreationTime,
    wgDescription,
    wgState,
  )
where

import qualified Network.AWS.Athena.Types.Name as Types
import qualified Network.AWS.Athena.Types.WorkGroupConfiguration as Types
import qualified Network.AWS.Athena.Types.WorkGroupDescriptionString as Types
import qualified Network.AWS.Athena.Types.WorkGroupState as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A workgroup, which contains a name, description, creation time, state, and other configuration, listed under 'WorkGroup$Configuration' . Each workgroup enables you to isolate queries for you or your group of users from other queries in the same account, to configure the query results location and the encryption configuration (known as workgroup settings), to enable sending query metrics to Amazon CloudWatch, and to establish per-query data usage control limits for all queries in a workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /See:/ 'mkWorkGroup' smart constructor.
data WorkGroup = WorkGroup'
  { -- | The workgroup name.
    name :: Types.Name,
    -- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
    configuration :: Core.Maybe Types.WorkGroupConfiguration,
    -- | The date and time the workgroup was created.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | The workgroup description.
    description :: Core.Maybe Types.WorkGroupDescriptionString,
    -- | The state of the workgroup: ENABLED or DISABLED.
    state :: Core.Maybe Types.WorkGroupState
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'WorkGroup' value with any optional fields omitted.
mkWorkGroup ::
  -- | 'name'
  Types.Name ->
  WorkGroup
mkWorkGroup name =
  WorkGroup'
    { name,
      configuration = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      state = Core.Nothing
    }

-- | The workgroup name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgName :: Lens.Lens' WorkGroup Types.Name
wgName = Lens.field @"name"
{-# DEPRECATED wgName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The configuration of the workgroup, which includes the location in Amazon S3 where query results are stored, the encryption configuration, if any, used for query results; whether the Amazon CloudWatch Metrics are enabled for the workgroup; whether workgroup settings override client-side settings; and the data usage limits for the amount of data scanned per query or per workgroup. The workgroup settings override is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgConfiguration :: Lens.Lens' WorkGroup (Core.Maybe Types.WorkGroupConfiguration)
wgConfiguration = Lens.field @"configuration"
{-# DEPRECATED wgConfiguration "Use generic-lens or generic-optics with 'configuration' instead." #-}

-- | The date and time the workgroup was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgCreationTime :: Lens.Lens' WorkGroup (Core.Maybe Core.NominalDiffTime)
wgCreationTime = Lens.field @"creationTime"
{-# DEPRECATED wgCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The workgroup description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgDescription :: Lens.Lens' WorkGroup (Core.Maybe Types.WorkGroupDescriptionString)
wgDescription = Lens.field @"description"
{-# DEPRECATED wgDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The state of the workgroup: ENABLED or DISABLED.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
wgState :: Lens.Lens' WorkGroup (Core.Maybe Types.WorkGroupState)
wgState = Lens.field @"state"
{-# DEPRECATED wgState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON WorkGroup where
  parseJSON =
    Core.withObject "WorkGroup" Core.$
      \x ->
        WorkGroup'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..:? "Configuration")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "Description")
          Core.<*> (x Core..:? "State")
