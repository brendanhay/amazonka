{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultConfigurationUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.ResultConfigurationUpdates
  ( ResultConfigurationUpdates (..)
  -- * Smart constructor
  , mkResultConfigurationUpdates
  -- * Lenses
  , rcuEncryptionConfiguration
  , rcuOutputLocation
  , rcuRemoveEncryptionConfiguration
  , rcuRemoveOutputLocation
  ) where

import qualified Network.AWS.Athena.Types.EncryptionConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The information about the updates in the query results, such as output location and encryption configuration for the query results.
--
-- /See:/ 'mkResultConfigurationUpdates' smart constructor.
data ResultConfigurationUpdates = ResultConfigurationUpdates'
  { encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration
    -- ^ The encryption configuration for the query results.
  , outputLocation :: Core.Maybe Core.Text
    -- ^ The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
  , removeEncryptionConfiguration :: Core.Maybe Core.Bool
    -- ^ If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
  , removeOutputLocation :: Core.Maybe Core.Bool
    -- ^ If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResultConfigurationUpdates' value with any optional fields omitted.
mkResultConfigurationUpdates
    :: ResultConfigurationUpdates
mkResultConfigurationUpdates
  = ResultConfigurationUpdates'{encryptionConfiguration =
                                  Core.Nothing,
                                outputLocation = Core.Nothing,
                                removeEncryptionConfiguration = Core.Nothing,
                                removeOutputLocation = Core.Nothing}

-- | The encryption configuration for the query results.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuEncryptionConfiguration :: Lens.Lens' ResultConfigurationUpdates (Core.Maybe Types.EncryptionConfiguration)
rcuEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE rcuEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuOutputLocation :: Lens.Lens' ResultConfigurationUpdates (Core.Maybe Core.Text)
rcuOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE rcuOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

-- | If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'removeEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuRemoveEncryptionConfiguration :: Lens.Lens' ResultConfigurationUpdates (Core.Maybe Core.Bool)
rcuRemoveEncryptionConfiguration = Lens.field @"removeEncryptionConfiguration"
{-# INLINEABLE rcuRemoveEncryptionConfiguration #-}
{-# DEPRECATED removeEncryptionConfiguration "Use generic-lens or generic-optics with 'removeEncryptionConfiguration' instead"  #-}

-- | If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'removeOutputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuRemoveOutputLocation :: Lens.Lens' ResultConfigurationUpdates (Core.Maybe Core.Bool)
rcuRemoveOutputLocation = Lens.field @"removeOutputLocation"
{-# INLINEABLE rcuRemoveOutputLocation #-}
{-# DEPRECATED removeOutputLocation "Use generic-lens or generic-optics with 'removeOutputLocation' instead"  #-}

instance Core.FromJSON ResultConfigurationUpdates where
        toJSON ResultConfigurationUpdates{..}
          = Core.object
              (Core.catMaybes
                 [("EncryptionConfiguration" Core..=) Core.<$>
                    encryptionConfiguration,
                  ("OutputLocation" Core..=) Core.<$> outputLocation,
                  ("RemoveEncryptionConfiguration" Core..=) Core.<$>
                    removeEncryptionConfiguration,
                  ("RemoveOutputLocation" Core..=) Core.<$> removeOutputLocation])
