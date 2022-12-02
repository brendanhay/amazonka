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
-- Module      : Amazonka.ResourceGroups.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResourceGroups.Types.ResourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains the ARN of a resource and its resource type.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The resource type of a resource, such as @AWS::EC2::Instance@.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The ARN of a resource.
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'resourceIdentifier_resourceType' - The resource type of a resource, such as @AWS::EC2::Instance@.
--
-- 'resourceArn', 'resourceIdentifier_resourceArn' - The ARN of a resource.
newResourceIdentifier ::
  ResourceIdentifier
newResourceIdentifier =
  ResourceIdentifier'
    { resourceType = Prelude.Nothing,
      resourceArn = Prelude.Nothing
    }

-- | The resource type of a resource, such as @AWS::EC2::Instance@.
resourceIdentifier_resourceType :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_resourceType = Lens.lens (\ResourceIdentifier' {resourceType} -> resourceType) (\s@ResourceIdentifier' {} a -> s {resourceType = a} :: ResourceIdentifier)

-- | The ARN of a resource.
resourceIdentifier_resourceArn :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_resourceArn = Lens.lens (\ResourceIdentifier' {resourceArn} -> resourceArn) (\s@ResourceIdentifier' {} a -> s {resourceArn = a} :: ResourceIdentifier)

instance Data.FromJSON ResourceIdentifier where
  parseJSON =
    Data.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Prelude.<$> (x Data..:? "ResourceType")
            Prelude.<*> (x Data..:? "ResourceArn")
      )

instance Prelude.Hashable ResourceIdentifier where
  hashWithSalt _salt ResourceIdentifier' {..} =
    _salt `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData ResourceIdentifier where
  rnf ResourceIdentifier' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourceArn
