{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ContainerOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ContainerOverrides where

import Network.AWS.Batch.Types.KeyValuePair
import Network.AWS.Batch.Types.ResourceRequirement
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The overrides that should be sent to a container.
--
--
--
-- /See:/ 'containerOverrides' smart constructor.
data ContainerOverrides = ContainerOverrides'
  { _coCommand ::
      !(Maybe [Text]),
    _coEnvironment :: !(Maybe [KeyValuePair]),
    _coResourceRequirements ::
      !(Maybe [ResourceRequirement]),
    _coInstanceType :: !(Maybe Text),
    _coMemory :: !(Maybe Int),
    _coVcpus :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCommand' - The command to send to the container that overrides the default command from the Docker image or the job definition.
--
-- * 'coEnvironment' - The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
--
-- * 'coResourceRequirements' - The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
--
-- * 'coInstanceType' - The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
--
-- * 'coMemory' - The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
--
-- * 'coVcpus' - The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
containerOverrides ::
  ContainerOverrides
containerOverrides =
  ContainerOverrides'
    { _coCommand = Nothing,
      _coEnvironment = Nothing,
      _coResourceRequirements = Nothing,
      _coInstanceType = Nothing,
      _coMemory = Nothing,
      _coVcpus = Nothing
    }

-- | The command to send to the container that overrides the default command from the Docker image or the job definition.
coCommand :: Lens' ContainerOverrides [Text]
coCommand = lens _coCommand (\s a -> s {_coCommand = a}) . _Default . _Coerce

-- | The environment variables to send to the container. You can add new environment variables, which are added to the container at launch, or you can override the existing environment variables from the Docker image or the job definition.
coEnvironment :: Lens' ContainerOverrides [KeyValuePair]
coEnvironment = lens _coEnvironment (\s a -> s {_coEnvironment = a}) . _Default . _Coerce

-- | The type and amount of a resource to assign to a container. This value overrides the value set in the job definition. Currently, the only supported resource is @GPU@ .
coResourceRequirements :: Lens' ContainerOverrides [ResourceRequirement]
coResourceRequirements = lens _coResourceRequirements (\s a -> s {_coResourceRequirements = a}) . _Default . _Coerce

-- | The instance type to use for a multi-node parallel job. This parameter is not valid for single-node container jobs.
coInstanceType :: Lens' ContainerOverrides (Maybe Text)
coInstanceType = lens _coInstanceType (\s a -> s {_coInstanceType = a})

-- | The number of MiB of memory reserved for the job. This value overrides the value set in the job definition.
coMemory :: Lens' ContainerOverrides (Maybe Int)
coMemory = lens _coMemory (\s a -> s {_coMemory = a})

-- | The number of vCPUs to reserve for the container. This value overrides the value set in the job definition.
coVcpus :: Lens' ContainerOverrides (Maybe Int)
coVcpus = lens _coVcpus (\s a -> s {_coVcpus = a})

instance Hashable ContainerOverrides

instance NFData ContainerOverrides

instance ToJSON ContainerOverrides where
  toJSON ContainerOverrides' {..} =
    object
      ( catMaybes
          [ ("command" .=) <$> _coCommand,
            ("environment" .=) <$> _coEnvironment,
            ("resourceRequirements" .=) <$> _coResourceRequirements,
            ("instanceType" .=) <$> _coInstanceType,
            ("memory" .=) <$> _coMemory,
            ("vcpus" .=) <$> _coVcpus
          ]
      )
