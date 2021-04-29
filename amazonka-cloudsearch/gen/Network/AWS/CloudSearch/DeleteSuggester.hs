{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudSearch.DeleteSuggester
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a suggester. For more information, see
-- <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/getting-suggestions.html Getting Search Suggestions>
-- in the /Amazon CloudSearch Developer Guide/.
module Network.AWS.CloudSearch.DeleteSuggester
  ( -- * Creating a Request
    DeleteSuggester (..),
    newDeleteSuggester,

    -- * Request Lenses
    deleteSuggester_domainName,
    deleteSuggester_suggesterName,

    -- * Destructuring the Response
    DeleteSuggesterResponse (..),
    newDeleteSuggesterResponse,

    -- * Response Lenses
    deleteSuggesterResponse_httpStatus,
    deleteSuggesterResponse_suggester,
  )
where

import Network.AWS.CloudSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the @DeleteSuggester@ operation.
-- Specifies the name of the domain you want to update and name of the
-- suggester you want to delete.
--
-- /See:/ 'newDeleteSuggester' smart constructor.
data DeleteSuggester = DeleteSuggester'
  { domainName :: Prelude.Text,
    -- | Specifies the name of the suggester you want to delete.
    suggesterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSuggester' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'deleteSuggester_domainName' - Undocumented member.
--
-- 'suggesterName', 'deleteSuggester_suggesterName' - Specifies the name of the suggester you want to delete.
newDeleteSuggester ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'suggesterName'
  Prelude.Text ->
  DeleteSuggester
newDeleteSuggester pDomainName_ pSuggesterName_ =
  DeleteSuggester'
    { domainName = pDomainName_,
      suggesterName = pSuggesterName_
    }

-- | Undocumented member.
deleteSuggester_domainName :: Lens.Lens' DeleteSuggester Prelude.Text
deleteSuggester_domainName = Lens.lens (\DeleteSuggester' {domainName} -> domainName) (\s@DeleteSuggester' {} a -> s {domainName = a} :: DeleteSuggester)

-- | Specifies the name of the suggester you want to delete.
deleteSuggester_suggesterName :: Lens.Lens' DeleteSuggester Prelude.Text
deleteSuggester_suggesterName = Lens.lens (\DeleteSuggester' {suggesterName} -> suggesterName) (\s@DeleteSuggester' {} a -> s {suggesterName = a} :: DeleteSuggester)

instance Prelude.AWSRequest DeleteSuggester where
  type Rs DeleteSuggester = DeleteSuggesterResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteSuggesterResult"
      ( \s h x ->
          DeleteSuggesterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..@ "Suggester")
      )

instance Prelude.Hashable DeleteSuggester

instance Prelude.NFData DeleteSuggester

instance Prelude.ToHeaders DeleteSuggester where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteSuggester where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteSuggester where
  toQuery DeleteSuggester' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteSuggester" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2013-01-01" :: Prelude.ByteString),
        "DomainName" Prelude.=: domainName,
        "SuggesterName" Prelude.=: suggesterName
      ]

-- | The result of a @DeleteSuggester@ request. Contains the status of the
-- deleted suggester.
--
-- /See:/ 'newDeleteSuggesterResponse' smart constructor.
data DeleteSuggesterResponse = DeleteSuggesterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The status of the suggester being deleted.
    suggester :: SuggesterStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteSuggesterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSuggesterResponse_httpStatus' - The response's http status code.
--
-- 'suggester', 'deleteSuggesterResponse_suggester' - The status of the suggester being deleted.
newDeleteSuggesterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'suggester'
  SuggesterStatus ->
  DeleteSuggesterResponse
newDeleteSuggesterResponse pHttpStatus_ pSuggester_ =
  DeleteSuggesterResponse'
    { httpStatus = pHttpStatus_,
      suggester = pSuggester_
    }

-- | The response's http status code.
deleteSuggesterResponse_httpStatus :: Lens.Lens' DeleteSuggesterResponse Prelude.Int
deleteSuggesterResponse_httpStatus = Lens.lens (\DeleteSuggesterResponse' {httpStatus} -> httpStatus) (\s@DeleteSuggesterResponse' {} a -> s {httpStatus = a} :: DeleteSuggesterResponse)

-- | The status of the suggester being deleted.
deleteSuggesterResponse_suggester :: Lens.Lens' DeleteSuggesterResponse SuggesterStatus
deleteSuggesterResponse_suggester = Lens.lens (\DeleteSuggesterResponse' {suggester} -> suggester) (\s@DeleteSuggesterResponse' {} a -> s {suggester = a} :: DeleteSuggesterResponse)

instance Prelude.NFData DeleteSuggesterResponse
