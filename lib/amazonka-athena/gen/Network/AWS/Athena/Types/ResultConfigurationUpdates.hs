{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultConfigurationUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultConfigurationUpdates
  ( ResultConfigurationUpdates (..),

    -- * Smart constructor
    mkResultConfigurationUpdates,

    -- * Lenses
    rcuRemoveOutputLocation,
    rcuRemoveEncryptionConfiguration,
    rcuEncryptionConfiguration,
    rcuOutputLocation,
  )
where

import Network.AWS.Athena.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The information about the updates in the query results, such as output location and encryption configuration for the query results.
--
-- /See:/ 'mkResultConfigurationUpdates' smart constructor.
data ResultConfigurationUpdates = ResultConfigurationUpdates'
  { -- | If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
    removeOutputLocation :: Lude.Maybe Lude.Bool,
    -- | If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
    removeEncryptionConfiguration :: Lude.Maybe Lude.Bool,
    -- | The encryption configuration for the query results.
    encryptionConfiguration :: Lude.Maybe EncryptionConfiguration,
    -- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
    outputLocation :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultConfigurationUpdates' with the minimum fields required to make a request.
--
-- * 'removeOutputLocation' - If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
-- * 'removeEncryptionConfiguration' - If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
-- * 'encryptionConfiguration' - The encryption configuration for the query results.
-- * 'outputLocation' - The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
mkResultConfigurationUpdates ::
  ResultConfigurationUpdates
mkResultConfigurationUpdates =
  ResultConfigurationUpdates'
    { removeOutputLocation = Lude.Nothing,
      removeEncryptionConfiguration = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      outputLocation = Lude.Nothing
    }

-- | If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'removeOutputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuRemoveOutputLocation :: Lens.Lens' ResultConfigurationUpdates (Lude.Maybe Lude.Bool)
rcuRemoveOutputLocation = Lens.lens (removeOutputLocation :: ResultConfigurationUpdates -> Lude.Maybe Lude.Bool) (\s a -> s {removeOutputLocation = a} :: ResultConfigurationUpdates)
{-# DEPRECATED rcuRemoveOutputLocation "Use generic-lens or generic-optics with 'removeOutputLocation' instead." #-}

-- | If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'removeEncryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuRemoveEncryptionConfiguration :: Lens.Lens' ResultConfigurationUpdates (Lude.Maybe Lude.Bool)
rcuRemoveEncryptionConfiguration = Lens.lens (removeEncryptionConfiguration :: ResultConfigurationUpdates -> Lude.Maybe Lude.Bool) (\s a -> s {removeEncryptionConfiguration = a} :: ResultConfigurationUpdates)
{-# DEPRECATED rcuRemoveEncryptionConfiguration "Use generic-lens or generic-optics with 'removeEncryptionConfiguration' instead." #-}

-- | The encryption configuration for the query results.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuEncryptionConfiguration :: Lens.Lens' ResultConfigurationUpdates (Lude.Maybe EncryptionConfiguration)
rcuEncryptionConfiguration = Lens.lens (encryptionConfiguration :: ResultConfigurationUpdates -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: ResultConfigurationUpdates)
{-# DEPRECATED rcuEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcuOutputLocation :: Lens.Lens' ResultConfigurationUpdates (Lude.Maybe Lude.Text)
rcuOutputLocation = Lens.lens (outputLocation :: ResultConfigurationUpdates -> Lude.Maybe Lude.Text) (\s a -> s {outputLocation = a} :: ResultConfigurationUpdates)
{-# DEPRECATED rcuOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

instance Lude.ToJSON ResultConfigurationUpdates where
  toJSON ResultConfigurationUpdates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RemoveOutputLocation" Lude..=) Lude.<$> removeOutputLocation,
            ("RemoveEncryptionConfiguration" Lude..=)
              Lude.<$> removeEncryptionConfiguration,
            ("EncryptionConfiguration" Lude..=)
              Lude.<$> encryptionConfiguration,
            ("OutputLocation" Lude..=) Lude.<$> outputLocation
          ]
      )
