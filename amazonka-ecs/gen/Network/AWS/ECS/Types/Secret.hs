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
-- Module      : Network.AWS.ECS.Types.Secret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Secret where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An object representing the secret to expose to your container. Secrets
-- can be exposed to a container in the following ways:
--
-- -   To inject sensitive data into your containers as environment
--     variables, use the @secrets@ container definition parameter.
--
-- -   To reference sensitive information in the log configuration of a
--     container, use the @secretOptions@ container definition parameter.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/specifying-sensitive-data.html Specifying Sensitive Data>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newSecret' smart constructor.
data Secret = Secret'
  { -- | The name of the secret.
    name :: Core.Text,
    -- | The secret to expose to the container. The supported values are either
    -- the full ARN of the AWS Secrets Manager secret or the full ARN of the
    -- parameter in the AWS Systems Manager Parameter Store.
    --
    -- If the AWS Systems Manager Parameter Store parameter exists in the same
    -- Region as the task you are launching, then you can use either the full
    -- ARN or name of the parameter. If the parameter exists in a different
    -- Region, then the full ARN must be specified.
    valueFrom :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Secret' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'secret_name' - The name of the secret.
--
-- 'valueFrom', 'secret_valueFrom' - The secret to expose to the container. The supported values are either
-- the full ARN of the AWS Secrets Manager secret or the full ARN of the
-- parameter in the AWS Systems Manager Parameter Store.
--
-- If the AWS Systems Manager Parameter Store parameter exists in the same
-- Region as the task you are launching, then you can use either the full
-- ARN or name of the parameter. If the parameter exists in a different
-- Region, then the full ARN must be specified.
newSecret ::
  -- | 'name'
  Core.Text ->
  -- | 'valueFrom'
  Core.Text ->
  Secret
newSecret pName_ pValueFrom_ =
  Secret' {name = pName_, valueFrom = pValueFrom_}

-- | The name of the secret.
secret_name :: Lens.Lens' Secret Core.Text
secret_name = Lens.lens (\Secret' {name} -> name) (\s@Secret' {} a -> s {name = a} :: Secret)

-- | The secret to expose to the container. The supported values are either
-- the full ARN of the AWS Secrets Manager secret or the full ARN of the
-- parameter in the AWS Systems Manager Parameter Store.
--
-- If the AWS Systems Manager Parameter Store parameter exists in the same
-- Region as the task you are launching, then you can use either the full
-- ARN or name of the parameter. If the parameter exists in a different
-- Region, then the full ARN must be specified.
secret_valueFrom :: Lens.Lens' Secret Core.Text
secret_valueFrom = Lens.lens (\Secret' {valueFrom} -> valueFrom) (\s@Secret' {} a -> s {valueFrom = a} :: Secret)

instance Core.FromJSON Secret where
  parseJSON =
    Core.withObject
      "Secret"
      ( \x ->
          Secret'
            Core.<$> (x Core..: "name") Core.<*> (x Core..: "valueFrom")
      )

instance Core.Hashable Secret

instance Core.NFData Secret

instance Core.ToJSON Secret where
  toJSON Secret' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("valueFrom" Core..= valueFrom)
          ]
      )
