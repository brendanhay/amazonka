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
-- Module      : Amazonka.CloudSearch.DefineSuggester
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CloudSearch.DefineSuggester
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

import Amazonka.CloudSearch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DefineSuggester@ operation.
-- Specifies the name of the domain you want to update and the suggester
-- configuration.
--
-- /See:/ 'newDefineSuggester' smart constructor.
data DefineSuggester = DefineSuggester'
  { domainName :: Prelude.Text,
    suggester :: Suggester
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'suggester'
  Suggester ->
  DefineSuggester
newDefineSuggester pDomainName_ pSuggester_ =
  DefineSuggester'
    { domainName = pDomainName_,
      suggester = pSuggester_
    }

-- | Undocumented member.
defineSuggester_domainName :: Lens.Lens' DefineSuggester Prelude.Text
defineSuggester_domainName = Lens.lens (\DefineSuggester' {domainName} -> domainName) (\s@DefineSuggester' {} a -> s {domainName = a} :: DefineSuggester)

-- | Undocumented member.
defineSuggester_suggester :: Lens.Lens' DefineSuggester Suggester
defineSuggester_suggester = Lens.lens (\DefineSuggester' {suggester} -> suggester) (\s@DefineSuggester' {} a -> s {suggester = a} :: DefineSuggester)

instance Core.AWSRequest DefineSuggester where
  type
    AWSResponse DefineSuggester =
      DefineSuggesterResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DefineSuggesterResult"
      ( \s h x ->
          DefineSuggesterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..@ "Suggester")
      )

instance Prelude.Hashable DefineSuggester where
  hashWithSalt _salt DefineSuggester' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` suggester

instance Prelude.NFData DefineSuggester where
  rnf DefineSuggester' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf suggester

instance Data.ToHeaders DefineSuggester where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DefineSuggester where
  toPath = Prelude.const "/"

instance Data.ToQuery DefineSuggester where
  toQuery DefineSuggester' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DefineSuggester" :: Prelude.ByteString),
        "Version"
          Data.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Data.=: domainName,
        "Suggester" Data.=: suggester
      ]

-- | The result of a @DefineSuggester@ request. Contains the status of the
-- newly-configured suggester.
--
-- /See:/ 'newDefineSuggesterResponse' smart constructor.
data DefineSuggesterResponse = DefineSuggesterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    suggester :: SuggesterStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'suggester'
  SuggesterStatus ->
  DefineSuggesterResponse
newDefineSuggesterResponse pHttpStatus_ pSuggester_ =
  DefineSuggesterResponse'
    { httpStatus = pHttpStatus_,
      suggester = pSuggester_
    }

-- | The response's http status code.
defineSuggesterResponse_httpStatus :: Lens.Lens' DefineSuggesterResponse Prelude.Int
defineSuggesterResponse_httpStatus = Lens.lens (\DefineSuggesterResponse' {httpStatus} -> httpStatus) (\s@DefineSuggesterResponse' {} a -> s {httpStatus = a} :: DefineSuggesterResponse)

-- | Undocumented member.
defineSuggesterResponse_suggester :: Lens.Lens' DefineSuggesterResponse SuggesterStatus
defineSuggesterResponse_suggester = Lens.lens (\DefineSuggesterResponse' {suggester} -> suggester) (\s@DefineSuggesterResponse' {} a -> s {suggester = a} :: DefineSuggesterResponse)

instance Prelude.NFData DefineSuggesterResponse where
  rnf DefineSuggesterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf suggester
