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
-- Module      : Amazonka.CognitoIdentity.DescribeIdentity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata related to the given identity, including when the
-- identity was created and any associated linked logins.
--
-- You must use AWS Developer credentials to call this API.
module Amazonka.CognitoIdentity.DescribeIdentity
  ( -- * Creating a Request
    DescribeIdentity (..),
    newDescribeIdentity,

    -- * Request Lenses
    describeIdentity_identityId,

    -- * Destructuring the Response
    IdentityDescription (..),
    newIdentityDescription,

    -- * Response Lenses
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_lastModifiedDate,
    identityDescription_logins,
  )
where

import Amazonka.CognitoIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Input to the @DescribeIdentity@ action.
--
-- /See:/ 'newDescribeIdentity' smart constructor.
data DescribeIdentity = DescribeIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityId', 'describeIdentity_identityId' - A unique identifier in the format REGION:GUID.
newDescribeIdentity ::
  -- | 'identityId'
  Prelude.Text ->
  DescribeIdentity
newDescribeIdentity pIdentityId_ =
  DescribeIdentity' {identityId = pIdentityId_}

-- | A unique identifier in the format REGION:GUID.
describeIdentity_identityId :: Lens.Lens' DescribeIdentity Prelude.Text
describeIdentity_identityId = Lens.lens (\DescribeIdentity' {identityId} -> identityId) (\s@DescribeIdentity' {} a -> s {identityId = a} :: DescribeIdentity)

instance Core.AWSRequest DescribeIdentity where
  type
    AWSResponse DescribeIdentity =
      IdentityDescription
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable DescribeIdentity where
  hashWithSalt _salt DescribeIdentity' {..} =
    _salt `Prelude.hashWithSalt` identityId

instance Prelude.NFData DescribeIdentity where
  rnf DescribeIdentity' {..} = Prelude.rnf identityId

instance Data.ToHeaders DescribeIdentity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSCognitoIdentityService.DescribeIdentity" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeIdentity where
  toJSON DescribeIdentity' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("IdentityId" Data..= identityId)]
      )

instance Data.ToPath DescribeIdentity where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeIdentity where
  toQuery = Prelude.const Prelude.mempty
