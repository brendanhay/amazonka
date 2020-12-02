{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultConfiguration where

import Network.AWS.Athena.Types.EncryptionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location in Amazon S3 where query results are stored and the encryption option, if any, used for query results. These are known as "client-side settings". If workgroup settings override client-side settings, then the query uses the workgroup settings.
--
--
--
-- /See:/ 'resultConfiguration' smart constructor.
data ResultConfiguration = ResultConfiguration'
  { _rcEncryptionConfiguration ::
      !(Maybe EncryptionConfiguration),
    _rcOutputLocation :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcEncryptionConfiguration' - If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'rcOutputLocation' - The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
resultConfiguration ::
  ResultConfiguration
resultConfiguration =
  ResultConfiguration'
    { _rcEncryptionConfiguration = Nothing,
      _rcOutputLocation = Nothing
    }

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information. This is a client-side setting. If workgroup settings override client-side settings, then the query uses the encryption configuration that is specified for the workgroup, and also uses the location for storing query results specified in the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' and <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
rcEncryptionConfiguration :: Lens' ResultConfiguration (Maybe EncryptionConfiguration)
rcEncryptionConfiguration = lens _rcEncryptionConfiguration (\s a -> s {_rcEncryptionConfiguration = a})

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . To run the query, you must specify the query results location using one of the ways: either for individual queries using either this setting (client-side), or in the workgroup, using 'WorkGroupConfiguration' . If none of them is set, Athena issues an error that no output location is provided. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> . If workgroup settings override client-side settings, then the query uses the settings specified for the workgroup. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
rcOutputLocation :: Lens' ResultConfiguration (Maybe Text)
rcOutputLocation = lens _rcOutputLocation (\s a -> s {_rcOutputLocation = a})

instance FromJSON ResultConfiguration where
  parseJSON =
    withObject
      "ResultConfiguration"
      ( \x ->
          ResultConfiguration'
            <$> (x .:? "EncryptionConfiguration") <*> (x .:? "OutputLocation")
      )

instance Hashable ResultConfiguration

instance NFData ResultConfiguration

instance ToJSON ResultConfiguration where
  toJSON ResultConfiguration' {..} =
    object
      ( catMaybes
          [ ("EncryptionConfiguration" .=) <$> _rcEncryptionConfiguration,
            ("OutputLocation" .=) <$> _rcOutputLocation
          ]
      )
