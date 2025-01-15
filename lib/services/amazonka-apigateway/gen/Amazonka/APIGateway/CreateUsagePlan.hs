{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.APIGateway.CreateUsagePlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage plan with the throttle and quota limits, as well as the
-- associated API stages, specified in the payload.
module Amazonka.APIGateway.CreateUsagePlan
  ( -- * Creating a Request
    CreateUsagePlan (..),
    newCreateUsagePlan,

    -- * Request Lenses
    createUsagePlan_apiStages,
    createUsagePlan_description,
    createUsagePlan_quota,
    createUsagePlan_tags,
    createUsagePlan_throttle,
    createUsagePlan_name,

    -- * Destructuring the Response
    UsagePlan (..),
    newUsagePlan,

    -- * Response Lenses
    usagePlan_apiStages,
    usagePlan_description,
    usagePlan_id,
    usagePlan_name,
    usagePlan_productCode,
    usagePlan_quota,
    usagePlan_tags,
    usagePlan_throttle,
  )
where

import Amazonka.APIGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The POST request to create a usage plan with the name, description,
-- throttle limits and quota limits, as well as the associated API stages,
-- specified in the payload.
--
-- /See:/ 'newCreateUsagePlan' smart constructor.
data CreateUsagePlan = CreateUsagePlan'
  { -- | The associated API stages of the usage plan.
    apiStages :: Prelude.Maybe [ApiStage],
    -- | The description of the usage plan.
    description :: Prelude.Maybe Prelude.Text,
    -- | The quota of the usage plan.
    quota :: Prelude.Maybe QuotaSettings,
    -- | The key-value map of strings. The valid character set is
    -- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
    -- start with @aws:@. The tag value can be up to 256 characters.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The throttling limits of the usage plan.
    throttle :: Prelude.Maybe ThrottleSettings,
    -- | The name of the usage plan.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateUsagePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiStages', 'createUsagePlan_apiStages' - The associated API stages of the usage plan.
--
-- 'description', 'createUsagePlan_description' - The description of the usage plan.
--
-- 'quota', 'createUsagePlan_quota' - The quota of the usage plan.
--
-- 'tags', 'createUsagePlan_tags' - The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
--
-- 'throttle', 'createUsagePlan_throttle' - The throttling limits of the usage plan.
--
-- 'name', 'createUsagePlan_name' - The name of the usage plan.
newCreateUsagePlan ::
  -- | 'name'
  Prelude.Text ->
  CreateUsagePlan
newCreateUsagePlan pName_ =
  CreateUsagePlan'
    { apiStages = Prelude.Nothing,
      description = Prelude.Nothing,
      quota = Prelude.Nothing,
      tags = Prelude.Nothing,
      throttle = Prelude.Nothing,
      name = pName_
    }

-- | The associated API stages of the usage plan.
createUsagePlan_apiStages :: Lens.Lens' CreateUsagePlan (Prelude.Maybe [ApiStage])
createUsagePlan_apiStages = Lens.lens (\CreateUsagePlan' {apiStages} -> apiStages) (\s@CreateUsagePlan' {} a -> s {apiStages = a} :: CreateUsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | The description of the usage plan.
createUsagePlan_description :: Lens.Lens' CreateUsagePlan (Prelude.Maybe Prelude.Text)
createUsagePlan_description = Lens.lens (\CreateUsagePlan' {description} -> description) (\s@CreateUsagePlan' {} a -> s {description = a} :: CreateUsagePlan)

-- | The quota of the usage plan.
createUsagePlan_quota :: Lens.Lens' CreateUsagePlan (Prelude.Maybe QuotaSettings)
createUsagePlan_quota = Lens.lens (\CreateUsagePlan' {quota} -> quota) (\s@CreateUsagePlan' {} a -> s {quota = a} :: CreateUsagePlan)

-- | The key-value map of strings. The valid character set is
-- [a-zA-Z+-=._:\/]. The tag key can be up to 128 characters and must not
-- start with @aws:@. The tag value can be up to 256 characters.
createUsagePlan_tags :: Lens.Lens' CreateUsagePlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createUsagePlan_tags = Lens.lens (\CreateUsagePlan' {tags} -> tags) (\s@CreateUsagePlan' {} a -> s {tags = a} :: CreateUsagePlan) Prelude.. Lens.mapping Lens.coerced

-- | The throttling limits of the usage plan.
createUsagePlan_throttle :: Lens.Lens' CreateUsagePlan (Prelude.Maybe ThrottleSettings)
createUsagePlan_throttle = Lens.lens (\CreateUsagePlan' {throttle} -> throttle) (\s@CreateUsagePlan' {} a -> s {throttle = a} :: CreateUsagePlan)

-- | The name of the usage plan.
createUsagePlan_name :: Lens.Lens' CreateUsagePlan Prelude.Text
createUsagePlan_name = Lens.lens (\CreateUsagePlan' {name} -> name) (\s@CreateUsagePlan' {} a -> s {name = a} :: CreateUsagePlan)

instance Core.AWSRequest CreateUsagePlan where
  type AWSResponse CreateUsagePlan = UsagePlan
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable CreateUsagePlan where
  hashWithSalt _salt CreateUsagePlan' {..} =
    _salt
      `Prelude.hashWithSalt` apiStages
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` quota
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` throttle
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateUsagePlan where
  rnf CreateUsagePlan' {..} =
    Prelude.rnf apiStages `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf quota `Prelude.seq`
          Prelude.rnf tags `Prelude.seq`
            Prelude.rnf throttle `Prelude.seq`
              Prelude.rnf name

instance Data.ToHeaders CreateUsagePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Accept"
              Data.=# ("application/json" :: Prelude.ByteString)
          ]
      )

instance Data.ToJSON CreateUsagePlan where
  toJSON CreateUsagePlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("apiStages" Data..=) Prelude.<$> apiStages,
            ("description" Data..=) Prelude.<$> description,
            ("quota" Data..=) Prelude.<$> quota,
            ("tags" Data..=) Prelude.<$> tags,
            ("throttle" Data..=) Prelude.<$> throttle,
            Prelude.Just ("name" Data..= name)
          ]
      )

instance Data.ToPath CreateUsagePlan where
  toPath = Prelude.const "/usageplans"

instance Data.ToQuery CreateUsagePlan where
  toQuery = Prelude.const Prelude.mempty
