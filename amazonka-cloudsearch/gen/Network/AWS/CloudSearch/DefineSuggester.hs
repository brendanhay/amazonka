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
-- Module      : Network.AWS.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Configures a suggester for a domain. A suggester enables you to display
-- possible matches before users finish typing their queries. When you
-- configure a suggester, you must specify the name of the text field you
-- want to search for possible matches and a unique name for the suggester.
-- For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DefineSuggester
  ( -- * Creating a Request
    DefineSuggester (..),
    newDefineSuggester,

    -- * Request Lenses
    defineSuggester_domainName,
    defineSuggester_suggester,

    -- * Destructuring the Response
    DefineSuggesterResponse (..),
    newDefineSuggesterResponse,

    -- * Response Lenses
    defineSuggesterResponse_httpStatus,
    defineSuggesterResponse_suggester,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DefineSuggester@ operation.
-- Specifies the name of the domain you want to update and the suggester
-- configuration.
--
-- /See:/ 'newDefineSuggester' smart constructor.
data DefineSuggester = DefineSuggester'
  { domainName :: Core.Text,
    suggester :: Suggester
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DefineSuggester' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'defineSuggester_domainName' - Undocumented member.
--
-- 'suggester', 'defineSuggester_suggester' - Undocumented member.
newDefineSuggester ::
  -- | 'domainName'
  Core.Text ->
  -- | 'suggester'
  Suggester ->
  DefineSuggester
newDefineSuggester pDomainName_ pSuggester_ =
  DefineSuggester'
    { domainName = pDomainName_,
      suggester = pSuggester_
    }

-- | Undocumented member.
defineSuggester_domainName :: Lens.Lens' DefineSuggester Core.Text
defineSuggester_domainName = Lens.lens (\DefineSuggester' {domainName} -> domainName) (\s@DefineSuggester' {} a -> s {domainName = a} :: DefineSuggester)

-- | Undocumented member.
defineSuggester_suggester :: Lens.Lens' DefineSuggester Suggester
defineSuggester_suggester = Lens.lens (\DefineSuggester' {suggester} -> suggester) (\s@DefineSuggester' {} a -> s {suggester = a} :: DefineSuggester)

instance Core.AWSRequest DefineSuggester where
  type
    AWSResponse DefineSuggester =
      DefineSuggesterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DefineSuggesterResult"
      ( \s h x ->
          DefineSuggesterResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..@ "Suggester")
      )

instance Core.Hashable DefineSuggester

instance Core.NFData DefineSuggester

instance Core.ToHeaders DefineSuggester where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DefineSuggester where
  toPath = Core.const "/"

instance Core.ToQuery DefineSuggester where
  toQuery DefineSuggester' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DefineSuggester" :: Core.ByteString),
        "Version" Core.=: ("2013-01-01" :: Core.ByteString),
        "DomainName" Core.=: domainName,
        "Suggester" Core.=: suggester
      ]

-- | The result of a @DefineSuggester@ request. Contains the status of the
-- newly-configured suggester.
--
-- /See:/ 'newDefineSuggesterResponse' smart constructor.
data DefineSuggesterResponse = DefineSuggesterResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    suggester :: SuggesterStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DefineSuggesterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'defineSuggesterResponse_httpStatus' - The response's http status code.
--
-- 'suggester', 'defineSuggesterResponse_suggester' - Undocumented member.
newDefineSuggesterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'suggester'
  SuggesterStatus ->
  DefineSuggesterResponse
newDefineSuggesterResponse pHttpStatus_ pSuggester_ =
  DefineSuggesterResponse'
    { httpStatus = pHttpStatus_,
      suggester = pSuggester_
    }

-- | The response's http status code.
defineSuggesterResponse_httpStatus :: Lens.Lens' DefineSuggesterResponse Core.Int
defineSuggesterResponse_httpStatus = Lens.lens (\DefineSuggesterResponse' {httpStatus} -> httpStatus) (\s@DefineSuggesterResponse' {} a -> s {httpStatus = a} :: DefineSuggesterResponse)

-- | Undocumented member.
defineSuggesterResponse_suggester :: Lens.Lens' DefineSuggesterResponse SuggesterStatus
defineSuggesterResponse_suggester = Lens.lens (\DefineSuggesterResponse' {suggester} -> suggester) (\s@DefineSuggesterResponse' {} a -> s {suggester = a} :: DefineSuggesterResponse)

instance Core.NFData DefineSuggesterResponse
