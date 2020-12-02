{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.ResultConfigurationUpdates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.ResultConfigurationUpdates where

import Network.AWS.Athena.Types.EncryptionConfiguration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The information about the updates in the query results, such as output location and encryption configuration for the query results.
--
--
--
-- /See:/ 'resultConfigurationUpdates' smart constructor.
data ResultConfigurationUpdates = ResultConfigurationUpdates'
  { _rcuRemoveOutputLocation ::
      !(Maybe Bool),
    _rcuRemoveEncryptionConfiguration ::
      !(Maybe Bool),
    _rcuEncryptionConfiguration ::
      !(Maybe EncryptionConfiguration),
    _rcuOutputLocation :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResultConfigurationUpdates' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcuRemoveOutputLocation' - If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'rcuRemoveEncryptionConfiguration' - If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
--
-- * 'rcuEncryptionConfiguration' - The encryption configuration for the query results.
--
-- * 'rcuOutputLocation' - The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
resultConfigurationUpdates ::
  ResultConfigurationUpdates
resultConfigurationUpdates =
  ResultConfigurationUpdates'
    { _rcuRemoveOutputLocation = Nothing,
      _rcuRemoveEncryptionConfiguration = Nothing,
      _rcuEncryptionConfiguration = Nothing,
      _rcuOutputLocation = Nothing
    }

-- | If set to "true", indicates that the previously-specified query results location (also known as a client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the OutputLocation in ResultConfigurationUpdates (the client-side setting), the OutputLocation in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
rcuRemoveOutputLocation :: Lens' ResultConfigurationUpdates (Maybe Bool)
rcuRemoveOutputLocation = lens _rcuRemoveOutputLocation (\s a -> s {_rcuRemoveOutputLocation = a})

-- | If set to "true", indicates that the previously-specified encryption configuration (also known as the client-side setting) for queries in this workgroup should be ignored and set to null. If set to "false" or not set, and a value is present in the EncryptionConfiguration in ResultConfigurationUpdates (the client-side setting), the EncryptionConfiguration in the workgroup's ResultConfiguration will be updated with the new value. For more information, see <https://docs.aws.amazon.com/athena/latest/ug/workgroups-settings-override.html Workgroup Settings Override Client-Side Settings> .
rcuRemoveEncryptionConfiguration :: Lens' ResultConfigurationUpdates (Maybe Bool)
rcuRemoveEncryptionConfiguration = lens _rcuRemoveEncryptionConfiguration (\s a -> s {_rcuRemoveEncryptionConfiguration = a})

-- | The encryption configuration for the query results.
rcuEncryptionConfiguration :: Lens' ResultConfigurationUpdates (Maybe EncryptionConfiguration)
rcuEncryptionConfiguration = lens _rcuEncryptionConfiguration (\s a -> s {_rcuEncryptionConfiguration = a})

-- | The location in Amazon S3 where your query results are stored, such as @s3://path/to/query/bucket/@ . For more information, see <https://docs.aws.amazon.com/athena/latest/ug/querying.html Query Results> If workgroup settings override client-side settings, then the query uses the location for the query results and the encryption configuration that are specified for the workgroup. The "workgroup settings override" is specified in EnforceWorkGroupConfiguration (true/false) in the WorkGroupConfiguration. See 'WorkGroupConfiguration$EnforceWorkGroupConfiguration' .
rcuOutputLocation :: Lens' ResultConfigurationUpdates (Maybe Text)
rcuOutputLocation = lens _rcuOutputLocation (\s a -> s {_rcuOutputLocation = a})

instance Hashable ResultConfigurationUpdates

instance NFData ResultConfigurationUpdates

instance ToJSON ResultConfigurationUpdates where
  toJSON ResultConfigurationUpdates' {..} =
    object
      ( catMaybes
          [ ("RemoveOutputLocation" .=) <$> _rcuRemoveOutputLocation,
            ("RemoveEncryptionConfiguration" .=)
              <$> _rcuRemoveEncryptionConfiguration,
            ("EncryptionConfiguration" .=) <$> _rcuEncryptionConfiguration,
            ("OutputLocation" .=) <$> _rcuOutputLocation
          ]
      )
