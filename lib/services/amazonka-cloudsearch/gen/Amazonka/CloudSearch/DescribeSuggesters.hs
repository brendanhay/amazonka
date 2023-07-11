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
-- Module      : Amazonka.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the suggesters configured for a domain. A suggester enables you to
-- display possible matches before users finish typing their queries. Can
-- be limited to specific suggesters by name. By default, shows all
-- suggesters and includes any pending changes to the configuration. Set
-- the @Deployed@ option to @true@ to show the active configuration and
-- exclude pending changes. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
module Amazonka.CloudSearch.DescribeSuggesters
  ( -- * Creating a Request
    DescribeSuggesters (..),
    newDescribeSuggesters,

    -- * Request Lenses
    describeSuggesters_deployed,
    describeSuggesters_suggesterNames,
    describeSuggesters_domainName,

    -- * Destructuring the Response
    DescribeSuggestersResponse (..),
    newDescribeSuggestersResponse,

    -- * Response Lenses
    describeSuggestersResponse_httpStatus,
    describeSuggestersResponse_suggesters,
  )
where

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeSuggester@ operation.
-- Specifies the name of the domain you want to describe. To restrict the
-- response to particular suggesters, specify the names of the suggesters
-- you want to describe. To show the active configuration and exclude any
-- pending changes, set the @Deployed@ option to @true@.
--
-- /See:/ 'newDescribeSuggesters' smart constructor.
data DescribeSuggesters = DescribeSuggesters'
  { -- | Whether to display the deployed configuration (@true@) or include any
    -- pending changes (@false@). Defaults to @false@.
    deployed :: Prelude.Maybe Prelude.Bool,
    -- | The suggesters you want to describe.
    suggesterNames :: Prelude.Maybe [Prelude.Text],
    -- | The name of the domain you want to describe.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSuggesters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deployed', 'describeSuggesters_deployed' - Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
--
-- 'suggesterNames', 'describeSuggesters_suggesterNames' - The suggesters you want to describe.
--
-- 'domainName', 'describeSuggesters_domainName' - The name of the domain you want to describe.
newDescribeSuggesters ::
  -- | 'domainName'
  Prelude.Text ->
  DescribeSuggesters
newDescribeSuggesters pDomainName_ =
  DescribeSuggesters'
    { deployed = Prelude.Nothing,
      suggesterNames = Prelude.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeSuggesters_deployed :: Lens.Lens' DescribeSuggesters (Prelude.Maybe Prelude.Bool)
describeSuggesters_deployed = Lens.lens (\DescribeSuggesters' {deployed} -> deployed) (\s@DescribeSuggesters' {} a -> s {deployed = a} :: DescribeSuggesters)

-- | The suggesters you want to describe.
describeSuggesters_suggesterNames :: Lens.Lens' DescribeSuggesters (Prelude.Maybe [Prelude.Text])
describeSuggesters_suggesterNames = Lens.lens (\DescribeSuggesters' {suggesterNames} -> suggesterNames) (\s@DescribeSuggesters' {} a -> s {suggesterNames = a} :: DescribeSuggesters) Prelude.. Lens.mapping Lens.coerced

-- | The name of the domain you want to describe.
describeSuggesters_domainName :: Lens.Lens' DescribeSuggesters Prelude.Text
describeSuggesters_domainName = Lens.lens (\DescribeSuggesters' {domainName} -> domainName) (\s@DescribeSuggesters' {} a -> s {domainName = a} :: DescribeSuggesters)

instance Core.AWSRequest DescribeSuggesters where
  type
    AWSResponse DescribeSuggesters =
      DescribeSuggestersResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeSuggestersResult"
      ( \s h x ->
          DescribeSuggestersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..@? "Suggesters"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Data.parseXMLList "member"
                        )
      )

instance Prelude.Hashable DescribeSuggesters where
  hashWithSalt _salt DescribeSuggesters' {..} =
    _salt
      `Prelude.hashWithSalt` deployed
      `Prelude.hashWithSalt` suggesterNames
      `Prelude.hashWithSalt` domainName

instance Prelude.NFData DescribeSuggesters where
  rnf DescribeSuggesters' {..} =
    Prelude.rnf deployed
      `Prelude.seq` Prelude.rnf suggesterNames
      `Prelude.seq` Prelude.rnf domainName

instance Data.ToHeaders DescribeSuggesters where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeSuggesters where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeSuggesters where
  toQuery DescribeSuggesters' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeSuggesters" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "Deployed" Data.=: deployed,
        "SuggesterNames"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> suggesterNames
            ),
        "DomainName" Data.=: domainName
      ]

-- | The result of a @DescribeSuggesters@ request.
--
-- /See:/ 'newDescribeSuggestersResponse' smart constructor.
data DescribeSuggestersResponse = DescribeSuggestersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The suggesters configured for the domain specified in the request.
    suggesters :: [SuggesterStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeSuggestersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeSuggestersResponse_httpStatus' - The response's http status code.
--
-- 'suggesters', 'describeSuggestersResponse_suggesters' - The suggesters configured for the domain specified in the request.
newDescribeSuggestersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeSuggestersResponse
newDescribeSuggestersResponse pHttpStatus_ =
  DescribeSuggestersResponse'
    { httpStatus =
        pHttpStatus_,
      suggesters = Prelude.mempty
    }

-- | The response's http status code.
describeSuggestersResponse_httpStatus :: Lens.Lens' DescribeSuggestersResponse Prelude.Int
describeSuggestersResponse_httpStatus = Lens.lens (\DescribeSuggestersResponse' {httpStatus} -> httpStatus) (\s@DescribeSuggestersResponse' {} a -> s {httpStatus = a} :: DescribeSuggestersResponse)

-- | The suggesters configured for the domain specified in the request.
describeSuggestersResponse_suggesters :: Lens.Lens' DescribeSuggestersResponse [SuggesterStatus]
describeSuggestersResponse_suggesters = Lens.lens (\DescribeSuggestersResponse' {suggesters} -> suggesters) (\s@DescribeSuggestersResponse' {} a -> s {suggesters = a} :: DescribeSuggestersResponse) Prelude.. Lens.coerced

instance Prelude.NFData DescribeSuggestersResponse where
  rnf DescribeSuggestersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf suggesters
