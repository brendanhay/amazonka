{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultConfiguration
  ( ResultConfiguration (..),

    -- * Smart constructor
    mkResultConfiguration,

    -- * Lenses
    rcEncryptionConfiguration,
    rcOutputLocation,
  )
where

import Network.AWS.Athena.Types.EncryptionConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the workgroup settings.
--
-- /See:/ 'mkResultConfiguration' smart constructor.
data ResultConfiguration = ResultConfiguration'
  { -- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
    encryptionConfiguration :: Lude.Maybe EncryptionConfiguration,
    -- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
    outputLocation :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResultConfiguration' with the minimum fields required to make a request.
--
-- * 'encryptionConfiguration' - If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
-- * 'outputLocation' - The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
mkResultConfiguration ::
  ResultConfiguration
mkResultConfiguration =
  ResultConfiguration'
    { encryptionConfiguration = Lude.Nothing,
      outputLocation = Lude.Nothing
    }

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcEncryptionConfiguration :: Lens.Lens' ResultConfiguration (Lude.Maybe EncryptionConfiguration)
rcEncryptionConfiguration = Lens.lens (encryptionConfiguration :: ResultConfiguration -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: ResultConfiguration)
{-# DEPRECATED rcEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
--
-- /Note:/ Consider using 'outputLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcOutputLocation :: Lens.Lens' ResultConfiguration (Lude.Maybe Lude.Text)
rcOutputLocation = Lens.lens (outputLocation :: ResultConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {outputLocation = a} :: ResultConfiguration)
{-# DEPRECATED rcOutputLocation "Use generic-lens or generic-optics with 'outputLocation' instead." #-}

instance Lude.FromJSON ResultConfiguration where
  parseJSON =
    Lude.withObject
      "ResultConfiguration"
      ( \x ->
          ResultConfiguration'
            Lude.<$> (x Lude..:? "EncryptionConfiguration")
            Lude.<*> (x Lude..:? "OutputLocation")
      )

instance Lude.ToJSON ResultConfiguration where
  toJSON ResultConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EncryptionConfiguration" Lude..=)
              Lude.<$> encryptionConfiguration,
            ("OutputLocation" Lude..=) Lude.<$> outputLocation
          ]
      )
