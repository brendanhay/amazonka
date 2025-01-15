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
-- Module      : Amazonka.LookoutEquipment.DeleteLabel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a label.
module Amazonka.LookoutEquipment.DeleteLabel
  ( -- * Creating a Request
    DeleteLabel (..),
    newDeleteLabel,

    -- * Request Lenses
    deleteLabel_labelGroupName,
    deleteLabel_labelId,

    -- * Destructuring the Response
    DeleteLabelResponse (..),
    newDeleteLabelResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLabel' smart constructor.
data DeleteLabel = DeleteLabel'
  { -- | The name of the label group that contains the label that you want to
    -- delete. Data in this field will be retained for service usage. Follow
    -- best practices for the security of your data.
    labelGroupName :: Prelude.Text,
    -- | The ID of the label that you want to delete.
    labelId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLabel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupName', 'deleteLabel_labelGroupName' - The name of the label group that contains the label that you want to
-- delete. Data in this field will be retained for service usage. Follow
-- best practices for the security of your data.
--
-- 'labelId', 'deleteLabel_labelId' - The ID of the label that you want to delete.
newDeleteLabel ::
  -- | 'labelGroupName'
  Prelude.Text ->
  -- | 'labelId'
  Prelude.Text ->
  DeleteLabel
newDeleteLabel pLabelGroupName_ pLabelId_ =
  DeleteLabel'
    { labelGroupName = pLabelGroupName_,
      labelId = pLabelId_
    }

-- | The name of the label group that contains the label that you want to
-- delete. Data in this field will be retained for service usage. Follow
-- best practices for the security of your data.
deleteLabel_labelGroupName :: Lens.Lens' DeleteLabel Prelude.Text
deleteLabel_labelGroupName = Lens.lens (\DeleteLabel' {labelGroupName} -> labelGroupName) (\s@DeleteLabel' {} a -> s {labelGroupName = a} :: DeleteLabel)

-- | The ID of the label that you want to delete.
deleteLabel_labelId :: Lens.Lens' DeleteLabel Prelude.Text
deleteLabel_labelId = Lens.lens (\DeleteLabel' {labelId} -> labelId) (\s@DeleteLabel' {} a -> s {labelId = a} :: DeleteLabel)

instance Core.AWSRequest DeleteLabel where
  type AWSResponse DeleteLabel = DeleteLabelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response = Response.receiveNull DeleteLabelResponse'

instance Prelude.Hashable DeleteLabel where
  hashWithSalt _salt DeleteLabel' {..} =
    _salt
      `Prelude.hashWithSalt` labelGroupName
      `Prelude.hashWithSalt` labelId

instance Prelude.NFData DeleteLabel where
  rnf DeleteLabel' {..} =
    Prelude.rnf labelGroupName `Prelude.seq`
      Prelude.rnf labelId

instance Data.ToHeaders DeleteLabel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DeleteLabel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLabel where
  toJSON DeleteLabel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LabelGroupName" Data..= labelGroupName),
            Prelude.Just ("LabelId" Data..= labelId)
          ]
      )

instance Data.ToPath DeleteLabel where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLabel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLabelResponse' smart constructor.
data DeleteLabelResponse = DeleteLabelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLabelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLabelResponse ::
  DeleteLabelResponse
newDeleteLabelResponse = DeleteLabelResponse'

instance Prelude.NFData DeleteLabelResponse where
  rnf _ = ()
