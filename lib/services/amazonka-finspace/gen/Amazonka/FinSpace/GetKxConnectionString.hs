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
-- Module      : Amazonka.FinSpace.GetKxConnectionString
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a connection string for a user to connect to a kdb cluster.
-- You must call this API using the same role that you have defined while
-- creating a user.
module Amazonka.FinSpace.GetKxConnectionString
  ( -- * Creating a Request
    GetKxConnectionString (..),
    newGetKxConnectionString,

    -- * Request Lenses
    getKxConnectionString_userArn,
    getKxConnectionString_environmentId,
    getKxConnectionString_clusterName,

    -- * Destructuring the Response
    GetKxConnectionStringResponse (..),
    newGetKxConnectionStringResponse,

    -- * Response Lenses
    getKxConnectionStringResponse_signedConnectionString,
    getKxConnectionStringResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FinSpace.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetKxConnectionString' smart constructor.
data GetKxConnectionString = GetKxConnectionString'
  { -- | The Amazon Resource Name (ARN) that identifies the user. For more
    -- information about ARNs and how to use ARNs in policies, see
    -- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
    -- /IAM User Guide/.
    userArn :: Prelude.Text,
    -- | A unique identifier for the kdb environment.
    environmentId :: Prelude.Text,
    -- | A name of the kdb cluster.
    clusterName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxConnectionString' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userArn', 'getKxConnectionString_userArn' - The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
--
-- 'environmentId', 'getKxConnectionString_environmentId' - A unique identifier for the kdb environment.
--
-- 'clusterName', 'getKxConnectionString_clusterName' - A name of the kdb cluster.
newGetKxConnectionString ::
  -- | 'userArn'
  Prelude.Text ->
  -- | 'environmentId'
  Prelude.Text ->
  -- | 'clusterName'
  Prelude.Text ->
  GetKxConnectionString
newGetKxConnectionString
  pUserArn_
  pEnvironmentId_
  pClusterName_ =
    GetKxConnectionString'
      { userArn = pUserArn_,
        environmentId = pEnvironmentId_,
        clusterName = pClusterName_
      }

-- | The Amazon Resource Name (ARN) that identifies the user. For more
-- information about ARNs and how to use ARNs in policies, see
-- <IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers> in the
-- /IAM User Guide/.
getKxConnectionString_userArn :: Lens.Lens' GetKxConnectionString Prelude.Text
getKxConnectionString_userArn = Lens.lens (\GetKxConnectionString' {userArn} -> userArn) (\s@GetKxConnectionString' {} a -> s {userArn = a} :: GetKxConnectionString)

-- | A unique identifier for the kdb environment.
getKxConnectionString_environmentId :: Lens.Lens' GetKxConnectionString Prelude.Text
getKxConnectionString_environmentId = Lens.lens (\GetKxConnectionString' {environmentId} -> environmentId) (\s@GetKxConnectionString' {} a -> s {environmentId = a} :: GetKxConnectionString)

-- | A name of the kdb cluster.
getKxConnectionString_clusterName :: Lens.Lens' GetKxConnectionString Prelude.Text
getKxConnectionString_clusterName = Lens.lens (\GetKxConnectionString' {clusterName} -> clusterName) (\s@GetKxConnectionString' {} a -> s {clusterName = a} :: GetKxConnectionString)

instance Core.AWSRequest GetKxConnectionString where
  type
    AWSResponse GetKxConnectionString =
      GetKxConnectionStringResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetKxConnectionStringResponse'
            Prelude.<$> (x Data..?> "signedConnectionString")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetKxConnectionString where
  hashWithSalt _salt GetKxConnectionString' {..} =
    _salt
      `Prelude.hashWithSalt` userArn
      `Prelude.hashWithSalt` environmentId
      `Prelude.hashWithSalt` clusterName

instance Prelude.NFData GetKxConnectionString where
  rnf GetKxConnectionString' {..} =
    Prelude.rnf userArn
      `Prelude.seq` Prelude.rnf environmentId
      `Prelude.seq` Prelude.rnf clusterName

instance Data.ToHeaders GetKxConnectionString where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetKxConnectionString where
  toPath GetKxConnectionString' {..} =
    Prelude.mconcat
      [ "/kx/environments/",
        Data.toBS environmentId,
        "/connectionString"
      ]

instance Data.ToQuery GetKxConnectionString where
  toQuery GetKxConnectionString' {..} =
    Prelude.mconcat
      [ "userArn" Data.=: userArn,
        "clusterName" Data.=: clusterName
      ]

-- | /See:/ 'newGetKxConnectionStringResponse' smart constructor.
data GetKxConnectionStringResponse = GetKxConnectionStringResponse'
  { -- | The signed connection string that you can use to connect to clusters.
    signedConnectionString :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetKxConnectionStringResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signedConnectionString', 'getKxConnectionStringResponse_signedConnectionString' - The signed connection string that you can use to connect to clusters.
--
-- 'httpStatus', 'getKxConnectionStringResponse_httpStatus' - The response's http status code.
newGetKxConnectionStringResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetKxConnectionStringResponse
newGetKxConnectionStringResponse pHttpStatus_ =
  GetKxConnectionStringResponse'
    { signedConnectionString =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The signed connection string that you can use to connect to clusters.
getKxConnectionStringResponse_signedConnectionString :: Lens.Lens' GetKxConnectionStringResponse (Prelude.Maybe Prelude.Text)
getKxConnectionStringResponse_signedConnectionString = Lens.lens (\GetKxConnectionStringResponse' {signedConnectionString} -> signedConnectionString) (\s@GetKxConnectionStringResponse' {} a -> s {signedConnectionString = a} :: GetKxConnectionStringResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
getKxConnectionStringResponse_httpStatus :: Lens.Lens' GetKxConnectionStringResponse Prelude.Int
getKxConnectionStringResponse_httpStatus = Lens.lens (\GetKxConnectionStringResponse' {httpStatus} -> httpStatus) (\s@GetKxConnectionStringResponse' {} a -> s {httpStatus = a} :: GetKxConnectionStringResponse)

instance Prelude.NFData GetKxConnectionStringResponse where
  rnf GetKxConnectionStringResponse' {..} =
    Prelude.rnf signedConnectionString
      `Prelude.seq` Prelude.rnf httpStatus
