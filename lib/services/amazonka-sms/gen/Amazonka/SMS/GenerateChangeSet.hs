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
-- Module      : Amazonka.SMS.GenerateChangeSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a target change set for a currently launched stack and writes
-- it to an Amazon S3 object in the customer’s Amazon S3 bucket.
module Amazonka.SMS.GenerateChangeSet
  ( -- * Creating a Request
    GenerateChangeSet (..),
    newGenerateChangeSet,

    -- * Request Lenses
    generateChangeSet_changesetFormat,
    generateChangeSet_appId,

    -- * Destructuring the Response
    GenerateChangeSetResponse (..),
    newGenerateChangeSetResponse,

    -- * Response Lenses
    generateChangeSetResponse_s3Location,
    generateChangeSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGenerateChangeSet' smart constructor.
data GenerateChangeSet = GenerateChangeSet'
  { -- | The format for the change set.
    changesetFormat :: Prelude.Maybe OutputFormat,
    -- | The ID of the application associated with the change set.
    appId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'changesetFormat', 'generateChangeSet_changesetFormat' - The format for the change set.
--
-- 'appId', 'generateChangeSet_appId' - The ID of the application associated with the change set.
newGenerateChangeSet ::
  GenerateChangeSet
newGenerateChangeSet =
  GenerateChangeSet'
    { changesetFormat =
        Prelude.Nothing,
      appId = Prelude.Nothing
    }

-- | The format for the change set.
generateChangeSet_changesetFormat :: Lens.Lens' GenerateChangeSet (Prelude.Maybe OutputFormat)
generateChangeSet_changesetFormat = Lens.lens (\GenerateChangeSet' {changesetFormat} -> changesetFormat) (\s@GenerateChangeSet' {} a -> s {changesetFormat = a} :: GenerateChangeSet)

-- | The ID of the application associated with the change set.
generateChangeSet_appId :: Lens.Lens' GenerateChangeSet (Prelude.Maybe Prelude.Text)
generateChangeSet_appId = Lens.lens (\GenerateChangeSet' {appId} -> appId) (\s@GenerateChangeSet' {} a -> s {appId = a} :: GenerateChangeSet)

instance Core.AWSRequest GenerateChangeSet where
  type
    AWSResponse GenerateChangeSet =
      GenerateChangeSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GenerateChangeSetResponse'
            Prelude.<$> (x Data..?> "s3Location")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GenerateChangeSet where
  hashWithSalt _salt GenerateChangeSet' {..} =
    _salt `Prelude.hashWithSalt` changesetFormat
      `Prelude.hashWithSalt` appId

instance Prelude.NFData GenerateChangeSet where
  rnf GenerateChangeSet' {..} =
    Prelude.rnf changesetFormat
      `Prelude.seq` Prelude.rnf appId

instance Data.ToHeaders GenerateChangeSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSServerMigrationService_V2016_10_24.GenerateChangeSet" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GenerateChangeSet where
  toJSON GenerateChangeSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("changesetFormat" Data..=)
              Prelude.<$> changesetFormat,
            ("appId" Data..=) Prelude.<$> appId
          ]
      )

instance Data.ToPath GenerateChangeSet where
  toPath = Prelude.const "/"

instance Data.ToQuery GenerateChangeSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGenerateChangeSetResponse' smart constructor.
data GenerateChangeSetResponse = GenerateChangeSetResponse'
  { -- | The location of the Amazon S3 object.
    s3Location :: Prelude.Maybe S3Location,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GenerateChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Location', 'generateChangeSetResponse_s3Location' - The location of the Amazon S3 object.
--
-- 'httpStatus', 'generateChangeSetResponse_httpStatus' - The response's http status code.
newGenerateChangeSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GenerateChangeSetResponse
newGenerateChangeSetResponse pHttpStatus_ =
  GenerateChangeSetResponse'
    { s3Location =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The location of the Amazon S3 object.
generateChangeSetResponse_s3Location :: Lens.Lens' GenerateChangeSetResponse (Prelude.Maybe S3Location)
generateChangeSetResponse_s3Location = Lens.lens (\GenerateChangeSetResponse' {s3Location} -> s3Location) (\s@GenerateChangeSetResponse' {} a -> s {s3Location = a} :: GenerateChangeSetResponse)

-- | The response's http status code.
generateChangeSetResponse_httpStatus :: Lens.Lens' GenerateChangeSetResponse Prelude.Int
generateChangeSetResponse_httpStatus = Lens.lens (\GenerateChangeSetResponse' {httpStatus} -> httpStatus) (\s@GenerateChangeSetResponse' {} a -> s {httpStatus = a} :: GenerateChangeSetResponse)

instance Prelude.NFData GenerateChangeSetResponse where
  rnf GenerateChangeSetResponse' {..} =
    Prelude.rnf s3Location
      `Prelude.seq` Prelude.rnf httpStatus
