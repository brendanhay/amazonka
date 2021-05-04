{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ContainerDatasetAction where

import Network.AWS.IoTAnalytics.Types.ResourceConfiguration
import Network.AWS.IoTAnalytics.Types.Variable
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information required to run the @containerAction@ to produce dataset
-- contents.
--
-- /See:/ 'newContainerDatasetAction' smart constructor.
data ContainerDatasetAction = ContainerDatasetAction'
  { -- | The values of variables used in the context of the execution of the
    -- containerized application (basically, parameters passed to the
    -- application). Each variable must have a name and a value given by one of
    -- @stringValue@, @datasetContentVersionValue@, or @outputFileUriValue@.
    variables :: Prelude.Maybe [Variable],
    -- | The ARN of the Docker container stored in your account. The Docker
    -- container contains an application and required support libraries and is
    -- used to generate dataset contents.
    image :: Prelude.Text,
    -- | The ARN of the role that gives permission to the system to access
    -- required resources to run the @containerAction@. This includes, at
    -- minimum, permission to retrieve the dataset contents that are the input
    -- to the containerized application.
    executionRoleArn :: Prelude.Text,
    -- | Configuration of the resource that executes the @containerAction@.
    resourceConfiguration :: ResourceConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ContainerDatasetAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variables', 'containerDatasetAction_variables' - The values of variables used in the context of the execution of the
-- containerized application (basically, parameters passed to the
-- application). Each variable must have a name and a value given by one of
-- @stringValue@, @datasetContentVersionValue@, or @outputFileUriValue@.
--
-- 'image', 'containerDatasetAction_image' - The ARN of the Docker container stored in your account. The Docker
-- container contains an application and required support libraries and is
-- used to generate dataset contents.
--
-- 'executionRoleArn', 'containerDatasetAction_executionRoleArn' - The ARN of the role that gives permission to the system to access
-- required resources to run the @containerAction@. This includes, at
-- minimum, permission to retrieve the dataset contents that are the input
-- to the containerized application.
--
-- 'resourceConfiguration', 'containerDatasetAction_resourceConfiguration' - Configuration of the resource that executes the @containerAction@.
newContainerDatasetAction ::
  -- | 'image'
  Prelude.Text ->
  -- | 'executionRoleArn'
  Prelude.Text ->
  -- | 'resourceConfiguration'
  ResourceConfiguration ->
  ContainerDatasetAction
newContainerDatasetAction
  pImage_
  pExecutionRoleArn_
  pResourceConfiguration_ =
    ContainerDatasetAction'
      { variables =
          Prelude.Nothing,
        image = pImage_,
        executionRoleArn = pExecutionRoleArn_,
        resourceConfiguration = pResourceConfiguration_
      }

-- | The values of variables used in the context of the execution of the
-- containerized application (basically, parameters passed to the
-- application). Each variable must have a name and a value given by one of
-- @stringValue@, @datasetContentVersionValue@, or @outputFileUriValue@.
containerDatasetAction_variables :: Lens.Lens' ContainerDatasetAction (Prelude.Maybe [Variable])
containerDatasetAction_variables = Lens.lens (\ContainerDatasetAction' {variables} -> variables) (\s@ContainerDatasetAction' {} a -> s {variables = a} :: ContainerDatasetAction) Prelude.. Lens.mapping Prelude._Coerce

-- | The ARN of the Docker container stored in your account. The Docker
-- container contains an application and required support libraries and is
-- used to generate dataset contents.
containerDatasetAction_image :: Lens.Lens' ContainerDatasetAction Prelude.Text
containerDatasetAction_image = Lens.lens (\ContainerDatasetAction' {image} -> image) (\s@ContainerDatasetAction' {} a -> s {image = a} :: ContainerDatasetAction)

-- | The ARN of the role that gives permission to the system to access
-- required resources to run the @containerAction@. This includes, at
-- minimum, permission to retrieve the dataset contents that are the input
-- to the containerized application.
containerDatasetAction_executionRoleArn :: Lens.Lens' ContainerDatasetAction Prelude.Text
containerDatasetAction_executionRoleArn = Lens.lens (\ContainerDatasetAction' {executionRoleArn} -> executionRoleArn) (\s@ContainerDatasetAction' {} a -> s {executionRoleArn = a} :: ContainerDatasetAction)

-- | Configuration of the resource that executes the @containerAction@.
containerDatasetAction_resourceConfiguration :: Lens.Lens' ContainerDatasetAction ResourceConfiguration
containerDatasetAction_resourceConfiguration = Lens.lens (\ContainerDatasetAction' {resourceConfiguration} -> resourceConfiguration) (\s@ContainerDatasetAction' {} a -> s {resourceConfiguration = a} :: ContainerDatasetAction)

instance Prelude.FromJSON ContainerDatasetAction where
  parseJSON =
    Prelude.withObject
      "ContainerDatasetAction"
      ( \x ->
          ContainerDatasetAction'
            Prelude.<$> ( x Prelude..:? "variables"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "image")
            Prelude.<*> (x Prelude..: "executionRoleArn")
            Prelude.<*> (x Prelude..: "resourceConfiguration")
      )

instance Prelude.Hashable ContainerDatasetAction

instance Prelude.NFData ContainerDatasetAction

instance Prelude.ToJSON ContainerDatasetAction where
  toJSON ContainerDatasetAction' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("variables" Prelude..=) Prelude.<$> variables,
            Prelude.Just ("image" Prelude..= image),
            Prelude.Just
              ("executionRoleArn" Prelude..= executionRoleArn),
            Prelude.Just
              ( "resourceConfiguration"
                  Prelude..= resourceConfiguration
              )
          ]
      )
