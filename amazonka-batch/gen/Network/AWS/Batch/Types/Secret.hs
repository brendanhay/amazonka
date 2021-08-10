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
-- Module      : Network.AWS.Batch.Types.Secret
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.Secret where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
-- <https://docs.aws.amazon.com/batch/latest/userguide/specifying-sensitive-data.html Specifying sensitive data>
-- in the /AWS Batch User Guide/.
--
-- /See:/ 'newSecret' smart constructor.
data Secret = Secret'
  { -- | The name of the secret.
    name :: Prelude.Text,
    -- | The secret to expose to the container. The supported values are either
    -- the full ARN of the AWS Secrets Manager secret or the full ARN of the
    -- parameter in the AWS Systems Manager Parameter Store.
    --
    -- If the AWS Systems Manager Parameter Store parameter exists in the same
    -- Region as the job you are launching, then you can use either the full
    -- ARN or name of the parameter. If the parameter exists in a different
    -- Region, then the full ARN must be specified.
    valueFrom :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- Region as the job you are launching, then you can use either the full
-- ARN or name of the parameter. If the parameter exists in a different
-- Region, then the full ARN must be specified.
newSecret ::
  -- | 'name'
  Prelude.Text ->
  -- | 'valueFrom'
  Prelude.Text ->
  Secret
newSecret pName_ pValueFrom_ =
  Secret' {name = pName_, valueFrom = pValueFrom_}

-- | The name of the secret.
secret_name :: Lens.Lens' Secret Prelude.Text
secret_name = Lens.lens (\Secret' {name} -> name) (\s@Secret' {} a -> s {name = a} :: Secret)

-- | The secret to expose to the container. The supported values are either
-- the full ARN of the AWS Secrets Manager secret or the full ARN of the
-- parameter in the AWS Systems Manager Parameter Store.
--
-- If the AWS Systems Manager Parameter Store parameter exists in the same
-- Region as the job you are launching, then you can use either the full
-- ARN or name of the parameter. If the parameter exists in a different
-- Region, then the full ARN must be specified.
secret_valueFrom :: Lens.Lens' Secret Prelude.Text
secret_valueFrom = Lens.lens (\Secret' {valueFrom} -> valueFrom) (\s@Secret' {} a -> s {valueFrom = a} :: Secret)

instance Core.FromJSON Secret where
  parseJSON =
    Core.withObject
      "Secret"
      ( \x ->
          Secret'
            Prelude.<$> (x Core..: "name")
            Prelude.<*> (x Core..: "valueFrom")
      )

instance Prelude.Hashable Secret

instance Prelude.NFData Secret

instance Core.ToJSON Secret where
  toJSON Secret' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("valueFrom" Core..= valueFrom)
          ]
      )
