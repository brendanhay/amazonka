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
-- Module      : Amazonka.Redshift.RejectDataShare
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- From a datashare consumer account, rejects the specified datashare.
module Amazonka.Redshift.RejectDataShare
  ( -- * Creating a Request
    RejectDataShare (..),
    newRejectDataShare,

    -- * Request Lenses
    rejectDataShare_dataShareArn,

    -- * Destructuring the Response
    DataShare (..),
    newDataShare,

    -- * Response Lenses
    dataShare_allowPubliclyAccessibleConsumers,
    dataShare_dataShareArn,
    dataShare_dataShareAssociations,
    dataShare_managedBy,
    dataShare_producerArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectDataShare' smart constructor.
data RejectDataShare = RejectDataShare'
  { -- | The Amazon Resource Name (ARN) of the datashare to reject.
    dataShareArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectDataShare' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataShareArn', 'rejectDataShare_dataShareArn' - The Amazon Resource Name (ARN) of the datashare to reject.
newRejectDataShare ::
  -- | 'dataShareArn'
  Prelude.Text ->
  RejectDataShare
newRejectDataShare pDataShareArn_ =
  RejectDataShare' {dataShareArn = pDataShareArn_}

-- | The Amazon Resource Name (ARN) of the datashare to reject.
rejectDataShare_dataShareArn :: Lens.Lens' RejectDataShare Prelude.Text
rejectDataShare_dataShareArn = Lens.lens (\RejectDataShare' {dataShareArn} -> dataShareArn) (\s@RejectDataShare' {} a -> s {dataShareArn = a} :: RejectDataShare)

instance Core.AWSRequest RejectDataShare where
  type AWSResponse RejectDataShare = DataShare
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RejectDataShareResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable RejectDataShare where
  hashWithSalt _salt RejectDataShare' {..} =
    _salt `Prelude.hashWithSalt` dataShareArn

instance Prelude.NFData RejectDataShare where
  rnf RejectDataShare' {..} = Prelude.rnf dataShareArn

instance Data.ToHeaders RejectDataShare where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RejectDataShare where
  toPath = Prelude.const "/"

instance Data.ToQuery RejectDataShare where
  toQuery RejectDataShare' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("RejectDataShare" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "DataShareArn" Data.=: dataShareArn
      ]
