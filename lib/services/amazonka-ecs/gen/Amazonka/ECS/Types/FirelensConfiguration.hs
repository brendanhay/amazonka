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
-- Module      : Amazonka.ECS.Types.FirelensConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.FirelensConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.FirelensConfigurationType
import qualified Amazonka.Prelude as Prelude

-- | The FireLens configuration for the container. This is used to specify
-- and configure a log router for container logs. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html Custom log routing>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newFirelensConfiguration' smart constructor.
data FirelensConfiguration = FirelensConfiguration'
  { -- | The options to use when configuring the log router. This field is
    -- optional and can be used to specify a custom configuration file or to
    -- add additional metadata, such as the task, task definition, cluster, and
    -- container instance details to the log event. If specified, the syntax to
    -- use is
    -- @\"options\":{\"enable-ecs-log-metadata\":\"true|false\",\"config-file-type:\"s3|file\",\"config-file-value\":\"arn:aws:s3:::mybucket\/fluent.conf|filepath\"}@.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a task definition that uses a FireLens configuration>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    --
    -- Tasks hosted on Fargate only support the @file@ configuration file type.
    options :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The log router to use. The valid values are @fluentd@ or @fluentbit@.
    type' :: FirelensConfigurationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FirelensConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'firelensConfiguration_options' - The options to use when configuring the log router. This field is
-- optional and can be used to specify a custom configuration file or to
-- add additional metadata, such as the task, task definition, cluster, and
-- container instance details to the log event. If specified, the syntax to
-- use is
-- @\"options\":{\"enable-ecs-log-metadata\":\"true|false\",\"config-file-type:\"s3|file\",\"config-file-value\":\"arn:aws:s3:::mybucket\/fluent.conf|filepath\"}@.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a task definition that uses a FireLens configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Tasks hosted on Fargate only support the @file@ configuration file type.
--
-- 'type'', 'firelensConfiguration_type' - The log router to use. The valid values are @fluentd@ or @fluentbit@.
newFirelensConfiguration ::
  -- | 'type''
  FirelensConfigurationType ->
  FirelensConfiguration
newFirelensConfiguration pType_ =
  FirelensConfiguration'
    { options = Prelude.Nothing,
      type' = pType_
    }

-- | The options to use when configuring the log router. This field is
-- optional and can be used to specify a custom configuration file or to
-- add additional metadata, such as the task, task definition, cluster, and
-- container instance details to the log event. If specified, the syntax to
-- use is
-- @\"options\":{\"enable-ecs-log-metadata\":\"true|false\",\"config-file-type:\"s3|file\",\"config-file-value\":\"arn:aws:s3:::mybucket\/fluent.conf|filepath\"}@.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/using_firelens.html#firelens-taskdef Creating a task definition that uses a FireLens configuration>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- Tasks hosted on Fargate only support the @file@ configuration file type.
firelensConfiguration_options :: Lens.Lens' FirelensConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
firelensConfiguration_options = Lens.lens (\FirelensConfiguration' {options} -> options) (\s@FirelensConfiguration' {} a -> s {options = a} :: FirelensConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The log router to use. The valid values are @fluentd@ or @fluentbit@.
firelensConfiguration_type :: Lens.Lens' FirelensConfiguration FirelensConfigurationType
firelensConfiguration_type = Lens.lens (\FirelensConfiguration' {type'} -> type') (\s@FirelensConfiguration' {} a -> s {type' = a} :: FirelensConfiguration)

instance Data.FromJSON FirelensConfiguration where
  parseJSON =
    Data.withObject
      "FirelensConfiguration"
      ( \x ->
          FirelensConfiguration'
            Prelude.<$> (x Data..:? "options" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "type")
      )

instance Prelude.Hashable FirelensConfiguration where
  hashWithSalt _salt FirelensConfiguration' {..} =
    _salt `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FirelensConfiguration where
  rnf FirelensConfiguration' {..} =
    Prelude.rnf options `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FirelensConfiguration where
  toJSON FirelensConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("options" Data..=) Prelude.<$> options,
            Prelude.Just ("type" Data..= type')
          ]
      )
