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
-- Module      : Network.AWS.CloudSearch.DescribeSuggesters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.CloudSearch.DescribeSuggesters
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

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
    deployed :: Core.Maybe Core.Bool,
    -- | The suggesters you want to describe.
    suggesterNames :: Core.Maybe [Core.Text],
    -- | The name of the domain you want to describe.
    domainName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeSuggesters
newDescribeSuggesters pDomainName_ =
  DescribeSuggesters'
    { deployed = Core.Nothing,
      suggesterNames = Core.Nothing,
      domainName = pDomainName_
    }

-- | Whether to display the deployed configuration (@true@) or include any
-- pending changes (@false@). Defaults to @false@.
describeSuggesters_deployed :: Lens.Lens' DescribeSuggesters (Core.Maybe Core.Bool)
describeSuggesters_deployed = Lens.lens (\DescribeSuggesters' {deployed} -> deployed) (\s@DescribeSuggesters' {} a -> s {deployed = a} :: DescribeSuggesters)

-- | The suggesters you want to describe.
describeSuggesters_suggesterNames :: Lens.Lens' DescribeSuggesters (Core.Maybe [Core.Text])
describeSuggesters_suggesterNames = Lens.lens (\DescribeSuggesters' {suggesterNames} -> suggesterNames) (\s@DescribeSuggesters' {} a -> s {suggesterNames = a} :: DescribeSuggesters) Core.. Lens.mapping Lens._Coerce

-- | The name of the domain you want to describe.
describeSuggesters_domainName :: Lens.Lens' DescribeSuggesters Core.Text
describeSuggesters_domainName = Lens.lens (\DescribeSuggesters' {domainName} -> domainName) (\s@DescribeSuggesters' {} a -> s {domainName = a} :: DescribeSuggesters)

instance Core.AWSRequest DescribeSuggesters where
  type
    AWSResponse DescribeSuggesters =
      DescribeSuggestersResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeSuggestersResult"
      ( \s h x ->
          DescribeSuggestersResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..@? "Suggesters" Core..!@ Core.mempty
                         Core.>>= Core.parseXMLList "member"
                     )
      )

instance Core.Hashable DescribeSuggesters

instance Core.NFData DescribeSuggesters

instance Core.ToHeaders DescribeSuggesters where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeSuggesters where
  toPath = Core.const "/"

instance Core.ToQuery DescribeSuggesters where
  toQuery DescribeSuggesters' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeSuggesters" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "Deployed" Core.=: deployed,
        "SuggesterNames"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Core.<$> suggesterNames),
        "DomainName" Core.=: domainName
      ]

-- | The result of a @DescribeSuggesters@ request.
--
-- /See:/ 'newDescribeSuggestersResponse' smart constructor.
data DescribeSuggestersResponse = DescribeSuggestersResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The suggesters configured for the domain specified in the request.
    suggesters :: [SuggesterStatus]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeSuggestersResponse
newDescribeSuggestersResponse pHttpStatus_ =
  DescribeSuggestersResponse'
    { httpStatus =
        pHttpStatus_,
      suggesters = Core.mempty
    }

-- | The response's http status code.
describeSuggestersResponse_httpStatus :: Lens.Lens' DescribeSuggestersResponse Core.Int
describeSuggestersResponse_httpStatus = Lens.lens (\DescribeSuggestersResponse' {httpStatus} -> httpStatus) (\s@DescribeSuggestersResponse' {} a -> s {httpStatus = a} :: DescribeSuggestersResponse)

-- | The suggesters configured for the domain specified in the request.
describeSuggestersResponse_suggesters :: Lens.Lens' DescribeSuggestersResponse [SuggesterStatus]
describeSuggestersResponse_suggesters = Lens.lens (\DescribeSuggestersResponse' {suggesters} -> suggesters) (\s@DescribeSuggestersResponse' {} a -> s {suggesters = a} :: DescribeSuggestersResponse) Core.. Lens._Coerce

instance Core.NFData DescribeSuggestersResponse
