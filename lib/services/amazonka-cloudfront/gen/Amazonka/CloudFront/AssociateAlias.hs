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
-- Module      : Amazonka.CloudFront.AssociateAlias
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an alias (also known as a CNAME or an alternate domain name)
-- with a CloudFront distribution.
--
-- With this operation you can move an alias that\'s already in use on a
-- CloudFront distribution to a different distribution in one step. This
-- prevents the downtime that could occur if you first remove the alias
-- from one distribution and then separately add the alias to another
-- distribution.
--
-- To use this operation to associate an alias with a distribution, you
-- provide the alias and the ID of the target distribution for the alias.
-- For more information, including how to set up the target distribution,
-- prerequisites that you must complete, and other restrictions, see
-- <https://docs.aws.amazon.com/AmazonCloudFront/latest/DeveloperGuide/CNAMEs.html#alternate-domain-names-move Moving an alternate domain name to a different distribution>
-- in the /Amazon CloudFront Developer Guide/.
module Amazonka.CloudFront.AssociateAlias
  ( -- * Creating a Request
    AssociateAlias (..),
    newAssociateAlias,

    -- * Request Lenses
    associateAlias_targetDistributionId,
    associateAlias_alias,

    -- * Destructuring the Response
    AssociateAliasResponse (..),
    newAssociateAliasResponse,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAssociateAlias' smart constructor.
data AssociateAlias = AssociateAlias'
  { -- | The ID of the distribution that you\'re associating the alias with.
    targetDistributionId :: Prelude.Text,
    -- | The alias (also known as a CNAME) to add to the target distribution.
    alias :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAlias' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetDistributionId', 'associateAlias_targetDistributionId' - The ID of the distribution that you\'re associating the alias with.
--
-- 'alias', 'associateAlias_alias' - The alias (also known as a CNAME) to add to the target distribution.
newAssociateAlias ::
  -- | 'targetDistributionId'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  AssociateAlias
newAssociateAlias pTargetDistributionId_ pAlias_ =
  AssociateAlias'
    { targetDistributionId =
        pTargetDistributionId_,
      alias = pAlias_
    }

-- | The ID of the distribution that you\'re associating the alias with.
associateAlias_targetDistributionId :: Lens.Lens' AssociateAlias Prelude.Text
associateAlias_targetDistributionId = Lens.lens (\AssociateAlias' {targetDistributionId} -> targetDistributionId) (\s@AssociateAlias' {} a -> s {targetDistributionId = a} :: AssociateAlias)

-- | The alias (also known as a CNAME) to add to the target distribution.
associateAlias_alias :: Lens.Lens' AssociateAlias Prelude.Text
associateAlias_alias = Lens.lens (\AssociateAlias' {alias} -> alias) (\s@AssociateAlias' {} a -> s {alias = a} :: AssociateAlias)

instance Core.AWSRequest AssociateAlias where
  type
    AWSResponse AssociateAlias =
      AssociateAliasResponse
  request overrides =
    Request.put (overrides defaultService)
  response =
    Response.receiveNull AssociateAliasResponse'

instance Prelude.Hashable AssociateAlias where
  hashWithSalt _salt AssociateAlias' {..} =
    _salt `Prelude.hashWithSalt` targetDistributionId
      `Prelude.hashWithSalt` alias

instance Prelude.NFData AssociateAlias where
  rnf AssociateAlias' {..} =
    Prelude.rnf targetDistributionId
      `Prelude.seq` Prelude.rnf alias

instance Data.ToHeaders AssociateAlias where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath AssociateAlias where
  toPath AssociateAlias' {..} =
    Prelude.mconcat
      [ "/2020-05-31/distribution/",
        Data.toBS targetDistributionId,
        "/associate-alias"
      ]

instance Data.ToQuery AssociateAlias where
  toQuery AssociateAlias' {..} =
    Prelude.mconcat ["Alias" Data.=: alias]

-- | /See:/ 'newAssociateAliasResponse' smart constructor.
data AssociateAliasResponse = AssociateAliasResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateAliasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateAliasResponse ::
  AssociateAliasResponse
newAssociateAliasResponse = AssociateAliasResponse'

instance Prelude.NFData AssociateAliasResponse where
  rnf _ = ()
