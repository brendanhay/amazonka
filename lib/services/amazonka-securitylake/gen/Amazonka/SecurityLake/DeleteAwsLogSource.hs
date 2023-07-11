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
-- Module      : Amazonka.SecurityLake.DeleteAwsLogSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a natively supported Amazon Web Service as an Amazon Security
-- Lake source. When you remove the source, Security Lake stops collecting
-- data from that source, and subscribers can no longer consume new data
-- from the source. Subscribers can still consume data that Security Lake
-- collected from the source before disablement.
--
-- You can choose any source type in any Amazon Web Services Region for
-- either accounts that are part of a trusted organization or standalone
-- accounts. At least one of the three dimensions is a mandatory input to
-- this API. However, you can supply any combination of the three
-- dimensions to this API.
--
-- By default, a dimension refers to the entire set. This is overridden
-- when you supply any one of the inputs. For instance, when you do not
-- specify members, the API disables all Security Lake member accounts for
-- sources. Similarly, when you do not specify Regions, Security Lake is
-- disabled for all the Regions where Security Lake is available as a
-- service.
--
-- When you don\'t provide a dimension, Security Lake assumes that the
-- missing dimension refers to the entire set. For example, if you don\'t
-- provide specific accounts, the API applies to the entire set of accounts
-- in your organization.
module Amazonka.SecurityLake.DeleteAwsLogSource
  ( -- * Creating a Request
    DeleteAwsLogSource (..),
    newDeleteAwsLogSource,

    -- * Request Lenses
    deleteAwsLogSource_disableAllDimensions,
    deleteAwsLogSource_disableSingleDimension,
    deleteAwsLogSource_disableTwoDimensions,
    deleteAwsLogSource_inputOrder,

    -- * Destructuring the Response
    DeleteAwsLogSourceResponse (..),
    newDeleteAwsLogSourceResponse,

    -- * Response Lenses
    deleteAwsLogSourceResponse_failed,
    deleteAwsLogSourceResponse_processing,
    deleteAwsLogSourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteAwsLogSource' smart constructor.
data DeleteAwsLogSource = DeleteAwsLogSource'
  { -- | Removes the specific Amazon Web Services sources from specific accounts
    -- and specific Regions.
    disableAllDimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text [Prelude.Text])),
    -- | Removes all Amazon Web Services sources from specific accounts or
    -- Regions.
    disableSingleDimension :: Prelude.Maybe [Prelude.Text],
    -- | Remove a specific Amazon Web Services source from specific accounts or
    -- Regions.
    disableTwoDimensions :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | This is a mandatory input. Specify the input order to disable dimensions
    -- in Security Lake, namely Region (Amazon Web Services Region code, source
    -- type, and member (account ID of a specific Amazon Web Services account).
    inputOrder :: [Dimension]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAwsLogSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disableAllDimensions', 'deleteAwsLogSource_disableAllDimensions' - Removes the specific Amazon Web Services sources from specific accounts
-- and specific Regions.
--
-- 'disableSingleDimension', 'deleteAwsLogSource_disableSingleDimension' - Removes all Amazon Web Services sources from specific accounts or
-- Regions.
--
-- 'disableTwoDimensions', 'deleteAwsLogSource_disableTwoDimensions' - Remove a specific Amazon Web Services source from specific accounts or
-- Regions.
--
-- 'inputOrder', 'deleteAwsLogSource_inputOrder' - This is a mandatory input. Specify the input order to disable dimensions
-- in Security Lake, namely Region (Amazon Web Services Region code, source
-- type, and member (account ID of a specific Amazon Web Services account).
newDeleteAwsLogSource ::
  DeleteAwsLogSource
newDeleteAwsLogSource =
  DeleteAwsLogSource'
    { disableAllDimensions =
        Prelude.Nothing,
      disableSingleDimension = Prelude.Nothing,
      disableTwoDimensions = Prelude.Nothing,
      inputOrder = Prelude.mempty
    }

-- | Removes the specific Amazon Web Services sources from specific accounts
-- and specific Regions.
deleteAwsLogSource_disableAllDimensions :: Lens.Lens' DeleteAwsLogSource (Prelude.Maybe (Prelude.HashMap Prelude.Text (Prelude.HashMap Prelude.Text [Prelude.Text])))
deleteAwsLogSource_disableAllDimensions = Lens.lens (\DeleteAwsLogSource' {disableAllDimensions} -> disableAllDimensions) (\s@DeleteAwsLogSource' {} a -> s {disableAllDimensions = a} :: DeleteAwsLogSource) Prelude.. Lens.mapping Lens.coerced

-- | Removes all Amazon Web Services sources from specific accounts or
-- Regions.
deleteAwsLogSource_disableSingleDimension :: Lens.Lens' DeleteAwsLogSource (Prelude.Maybe [Prelude.Text])
deleteAwsLogSource_disableSingleDimension = Lens.lens (\DeleteAwsLogSource' {disableSingleDimension} -> disableSingleDimension) (\s@DeleteAwsLogSource' {} a -> s {disableSingleDimension = a} :: DeleteAwsLogSource) Prelude.. Lens.mapping Lens.coerced

-- | Remove a specific Amazon Web Services source from specific accounts or
-- Regions.
deleteAwsLogSource_disableTwoDimensions :: Lens.Lens' DeleteAwsLogSource (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
deleteAwsLogSource_disableTwoDimensions = Lens.lens (\DeleteAwsLogSource' {disableTwoDimensions} -> disableTwoDimensions) (\s@DeleteAwsLogSource' {} a -> s {disableTwoDimensions = a} :: DeleteAwsLogSource) Prelude.. Lens.mapping Lens.coerced

-- | This is a mandatory input. Specify the input order to disable dimensions
-- in Security Lake, namely Region (Amazon Web Services Region code, source
-- type, and member (account ID of a specific Amazon Web Services account).
deleteAwsLogSource_inputOrder :: Lens.Lens' DeleteAwsLogSource [Dimension]
deleteAwsLogSource_inputOrder = Lens.lens (\DeleteAwsLogSource' {inputOrder} -> inputOrder) (\s@DeleteAwsLogSource' {} a -> s {inputOrder = a} :: DeleteAwsLogSource) Prelude.. Lens.coerced

instance Core.AWSRequest DeleteAwsLogSource where
  type
    AWSResponse DeleteAwsLogSource =
      DeleteAwsLogSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteAwsLogSourceResponse'
            Prelude.<$> (x Data..?> "failed" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "processing" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteAwsLogSource where
  hashWithSalt _salt DeleteAwsLogSource' {..} =
    _salt
      `Prelude.hashWithSalt` disableAllDimensions
      `Prelude.hashWithSalt` disableSingleDimension
      `Prelude.hashWithSalt` disableTwoDimensions
      `Prelude.hashWithSalt` inputOrder

instance Prelude.NFData DeleteAwsLogSource where
  rnf DeleteAwsLogSource' {..} =
    Prelude.rnf disableAllDimensions
      `Prelude.seq` Prelude.rnf disableSingleDimension
      `Prelude.seq` Prelude.rnf disableTwoDimensions
      `Prelude.seq` Prelude.rnf inputOrder

instance Data.ToHeaders DeleteAwsLogSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteAwsLogSource where
  toJSON DeleteAwsLogSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("disableAllDimensions" Data..=)
              Prelude.<$> disableAllDimensions,
            ("disableSingleDimension" Data..=)
              Prelude.<$> disableSingleDimension,
            ("disableTwoDimensions" Data..=)
              Prelude.<$> disableTwoDimensions,
            Prelude.Just ("inputOrder" Data..= inputOrder)
          ]
      )

instance Data.ToPath DeleteAwsLogSource where
  toPath = Prelude.const "/v1/logsources/aws/delete"

instance Data.ToQuery DeleteAwsLogSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteAwsLogSourceResponse' smart constructor.
data DeleteAwsLogSourceResponse = DeleteAwsLogSourceResponse'
  { -- | Deletion of the Amazon Web Services sources failed as the account is not
    -- a part of the organization.
    failed :: Prelude.Maybe [Prelude.Text],
    -- | Deletion of the Amazon Web Services sources is in progress.
    processing :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteAwsLogSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failed', 'deleteAwsLogSourceResponse_failed' - Deletion of the Amazon Web Services sources failed as the account is not
-- a part of the organization.
--
-- 'processing', 'deleteAwsLogSourceResponse_processing' - Deletion of the Amazon Web Services sources is in progress.
--
-- 'httpStatus', 'deleteAwsLogSourceResponse_httpStatus' - The response's http status code.
newDeleteAwsLogSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteAwsLogSourceResponse
newDeleteAwsLogSourceResponse pHttpStatus_ =
  DeleteAwsLogSourceResponse'
    { failed =
        Prelude.Nothing,
      processing = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Deletion of the Amazon Web Services sources failed as the account is not
-- a part of the organization.
deleteAwsLogSourceResponse_failed :: Lens.Lens' DeleteAwsLogSourceResponse (Prelude.Maybe [Prelude.Text])
deleteAwsLogSourceResponse_failed = Lens.lens (\DeleteAwsLogSourceResponse' {failed} -> failed) (\s@DeleteAwsLogSourceResponse' {} a -> s {failed = a} :: DeleteAwsLogSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | Deletion of the Amazon Web Services sources is in progress.
deleteAwsLogSourceResponse_processing :: Lens.Lens' DeleteAwsLogSourceResponse (Prelude.Maybe [Prelude.Text])
deleteAwsLogSourceResponse_processing = Lens.lens (\DeleteAwsLogSourceResponse' {processing} -> processing) (\s@DeleteAwsLogSourceResponse' {} a -> s {processing = a} :: DeleteAwsLogSourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
deleteAwsLogSourceResponse_httpStatus :: Lens.Lens' DeleteAwsLogSourceResponse Prelude.Int
deleteAwsLogSourceResponse_httpStatus = Lens.lens (\DeleteAwsLogSourceResponse' {httpStatus} -> httpStatus) (\s@DeleteAwsLogSourceResponse' {} a -> s {httpStatus = a} :: DeleteAwsLogSourceResponse)

instance Prelude.NFData DeleteAwsLogSourceResponse where
  rnf DeleteAwsLogSourceResponse' {..} =
    Prelude.rnf failed
      `Prelude.seq` Prelude.rnf processing
      `Prelude.seq` Prelude.rnf httpStatus
