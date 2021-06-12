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
-- Module      : Network.AWS.CognitoIdentity.DescribeIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata related to the given identity, including when the
-- identity was created and any associated linked logins.
--
-- You must use AWS Developer credentials to call this API.
module Network.AWS.CognitoIdentity.DescribeIdentity
  ( -- * Creating a Request
    DescribeIdentity (..),
    newDescribeIdentity,

    -- * Request Lenses
    describeIdentity_identityId,

    -- * Destructuring the Response
    IdentityDescription (..),
    newIdentityDescription,

    -- * Response Lenses
    identityDescription_lastModifiedDate,
    identityDescription_creationDate,
    identityDescription_identityId,
    identityDescription_logins,
  )
where

import Network.AWS.CognitoIdentity.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input to the @DescribeIdentity@ action.
--
-- /See:/ 'newDescribeIdentity' smart constructor.
data DescribeIdentity = DescribeIdentity'
  { -- | A unique identifier in the format REGION:GUID.
    identityId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeIdentity
newDescribeIdentity pIdentityId_ =
  DescribeIdentity' {identityId = pIdentityId_}

-- | A unique identifier in the format REGION:GUID.
describeIdentity_identityId :: Lens.Lens' DescribeIdentity Core.Text
describeIdentity_identityId = Lens.lens (\DescribeIdentity' {identityId} -> identityId) (\s@DescribeIdentity' {} a -> s {identityId = a} :: DescribeIdentity)

instance Core.AWSRequest DescribeIdentity where
  type
    AWSResponse DescribeIdentity =
      IdentityDescription
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable DescribeIdentity

instance Core.NFData DescribeIdentity

instance Core.ToHeaders DescribeIdentity where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSCognitoIdentityService.DescribeIdentity" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeIdentity where
  toJSON DescribeIdentity' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("IdentityId" Core..= identityId)]
      )

instance Core.ToPath DescribeIdentity where
  toPath = Core.const "/"

instance Core.ToQuery DescribeIdentity where
  toQuery = Core.const Core.mempty
