{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.ResultConfiguration
  ( ResultConfiguration (..)
  -- * Smart constructor
  , mkResultConfiguration
  -- * Lenses
  , rcEncryptionConfiguration
  , rcOutputLocation
  ) where

import qualified Network.AWS.Athena.Types.EncryptionConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the workgroup settings.
--
-- /See:/ 'mkResultConfiguration' smart constructor.
data ResultConfiguration = ResultConfiguration'
  { encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration
    -- ^ If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
  , outputLocation :: Core.Maybe Core.Text
    -- ^ The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResultConfiguration' value with any optional fields omitted.
mkResultConfiguration
    :: ResultConfiguration
mkResultConfiguration
  = ResultConfiguration'{encryptionConfiguration = Core.Nothing,
                         outputLocation = Core.Nothing}

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcEncryptionConfiguration :: Lens.Lens' ResultConfiguration (Core.Maybe Types.EncryptionConfiguration)
rcEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# INLINEABLE rcEncryptionConfiguration #-}
{-# DEPRECATED encryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead"  #-}

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcOutputLocation :: Lens.Lens' ResultConfiguration (Core.Maybe Core.Text)
rcOutputLocation = Lens.field @"outputLocation"
{-# INLINEABLE rcOutputLocation #-}
{-# DEPRECATED outputLocation "Use generic-lens or generic-optics with 'outputLocation' instead"  #-}

instance Core.FromJSON ResultConfiguration where
        toJSON ResultConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("EncryptionConfiguration" Core..=) Core.<$>
                    encryptionConfiguration,
                  ("OutputLocation" Core..=) Core.<$> outputLocation])

instance Core.FromJSON ResultConfiguration where
        parseJSON
          = Core.withObject "ResultConfiguration" Core.$
              \ x ->
                ResultConfiguration' Core.<$>
                  (x Core..:? "EncryptionConfiguration") Core.<*>
                    x Core..:? "OutputLocation"
