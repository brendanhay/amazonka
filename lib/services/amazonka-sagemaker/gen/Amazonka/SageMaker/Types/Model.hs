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
-- Module      : Amazonka.SageMaker.Types.Model
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Model where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ContainerDefinition
import Amazonka.SageMaker.Types.InferenceExecutionConfig
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.VpcConfig

-- | The properties of a model as returned by the Search API.
--
-- /See:/ 'newModel' smart constructor.
data Model = Model'
  { -- | The containers in the inference pipeline.
    containers :: Prelude.Maybe [ContainerDefinition],
    -- | A timestamp that indicates when the model was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Isolates the model container. No inbound or outbound network calls can
    -- be made to or from the model container.
    enableNetworkIsolation :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the IAM role that you specified for
    -- the model.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    inferenceExecutionConfig :: Prelude.Maybe InferenceExecutionConfig,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the model.
    modelName :: Prelude.Maybe Prelude.Text,
    primaryContainer :: Prelude.Maybe ContainerDefinition,
    -- | A list of key-value pairs associated with the model. For more
    -- information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    vpcConfig :: Prelude.Maybe VpcConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Model' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containers', 'model_containers' - The containers in the inference pipeline.
--
-- 'creationTime', 'model_creationTime' - A timestamp that indicates when the model was created.
--
-- 'enableNetworkIsolation', 'model_enableNetworkIsolation' - Isolates the model container. No inbound or outbound network calls can
-- be made to or from the model container.
--
-- 'executionRoleArn', 'model_executionRoleArn' - The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the model.
--
-- 'inferenceExecutionConfig', 'model_inferenceExecutionConfig' - Undocumented member.
--
-- 'modelArn', 'model_modelArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'modelName', 'model_modelName' - The name of the model.
--
-- 'primaryContainer', 'model_primaryContainer' - Undocumented member.
--
-- 'tags', 'model_tags' - A list of key-value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'vpcConfig', 'model_vpcConfig' - Undocumented member.
newModel ::
  Model
newModel =
  Model'
    { containers = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      enableNetworkIsolation = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      inferenceExecutionConfig = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      modelName = Prelude.Nothing,
      primaryContainer = Prelude.Nothing,
      tags = Prelude.Nothing,
      vpcConfig = Prelude.Nothing
    }

-- | The containers in the inference pipeline.
model_containers :: Lens.Lens' Model (Prelude.Maybe [ContainerDefinition])
model_containers = Lens.lens (\Model' {containers} -> containers) (\s@Model' {} a -> s {containers = a} :: Model) Prelude.. Lens.mapping Lens.coerced

-- | A timestamp that indicates when the model was created.
model_creationTime :: Lens.Lens' Model (Prelude.Maybe Prelude.UTCTime)
model_creationTime = Lens.lens (\Model' {creationTime} -> creationTime) (\s@Model' {} a -> s {creationTime = a} :: Model) Prelude.. Lens.mapping Data._Time

-- | Isolates the model container. No inbound or outbound network calls can
-- be made to or from the model container.
model_enableNetworkIsolation :: Lens.Lens' Model (Prelude.Maybe Prelude.Bool)
model_enableNetworkIsolation = Lens.lens (\Model' {enableNetworkIsolation} -> enableNetworkIsolation) (\s@Model' {} a -> s {enableNetworkIsolation = a} :: Model)

-- | The Amazon Resource Name (ARN) of the IAM role that you specified for
-- the model.
model_executionRoleArn :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_executionRoleArn = Lens.lens (\Model' {executionRoleArn} -> executionRoleArn) (\s@Model' {} a -> s {executionRoleArn = a} :: Model)

-- | Undocumented member.
model_inferenceExecutionConfig :: Lens.Lens' Model (Prelude.Maybe InferenceExecutionConfig)
model_inferenceExecutionConfig = Lens.lens (\Model' {inferenceExecutionConfig} -> inferenceExecutionConfig) (\s@Model' {} a -> s {inferenceExecutionConfig = a} :: Model)

-- | The Amazon Resource Name (ARN) of the model.
model_modelArn :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelArn = Lens.lens (\Model' {modelArn} -> modelArn) (\s@Model' {} a -> s {modelArn = a} :: Model)

-- | The name of the model.
model_modelName :: Lens.Lens' Model (Prelude.Maybe Prelude.Text)
model_modelName = Lens.lens (\Model' {modelName} -> modelName) (\s@Model' {} a -> s {modelName = a} :: Model)

-- | Undocumented member.
model_primaryContainer :: Lens.Lens' Model (Prelude.Maybe ContainerDefinition)
model_primaryContainer = Lens.lens (\Model' {primaryContainer} -> primaryContainer) (\s@Model' {} a -> s {primaryContainer = a} :: Model)

-- | A list of key-value pairs associated with the model. For more
-- information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
model_tags :: Lens.Lens' Model (Prelude.Maybe [Tag])
model_tags = Lens.lens (\Model' {tags} -> tags) (\s@Model' {} a -> s {tags = a} :: Model) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
model_vpcConfig :: Lens.Lens' Model (Prelude.Maybe VpcConfig)
model_vpcConfig = Lens.lens (\Model' {vpcConfig} -> vpcConfig) (\s@Model' {} a -> s {vpcConfig = a} :: Model)

instance Data.FromJSON Model where
  parseJSON =
    Data.withObject
      "Model"
      ( \x ->
          Model'
            Prelude.<$> (x Data..:? "Containers" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "EnableNetworkIsolation")
            Prelude.<*> (x Data..:? "ExecutionRoleArn")
            Prelude.<*> (x Data..:? "InferenceExecutionConfig")
            Prelude.<*> (x Data..:? "ModelArn")
            Prelude.<*> (x Data..:? "ModelName")
            Prelude.<*> (x Data..:? "PrimaryContainer")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "VpcConfig")
      )

instance Prelude.Hashable Model where
  hashWithSalt _salt Model' {..} =
    _salt
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` enableNetworkIsolation
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` inferenceExecutionConfig
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` primaryContainer
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` vpcConfig

instance Prelude.NFData Model where
  rnf Model' {..} =
    Prelude.rnf containers `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf enableNetworkIsolation `Prelude.seq`
          Prelude.rnf executionRoleArn `Prelude.seq`
            Prelude.rnf inferenceExecutionConfig `Prelude.seq`
              Prelude.rnf modelArn `Prelude.seq`
                Prelude.rnf modelName `Prelude.seq`
                  Prelude.rnf primaryContainer `Prelude.seq`
                    Prelude.rnf tags `Prelude.seq`
                      Prelude.rnf vpcConfig
