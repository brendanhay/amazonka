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
-- Module      : Amazonka.LookoutEquipment.DeleteLabelGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a group of labels.
module Amazonka.LookoutEquipment.DeleteLabelGroup
  ( -- * Creating a Request
    DeleteLabelGroup (..),
    newDeleteLabelGroup,

    -- * Request Lenses
    deleteLabelGroup_labelGroupName,

    -- * Destructuring the Response
    DeleteLabelGroupResponse (..),
    newDeleteLabelGroupResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteLabelGroup' smart constructor.
data DeleteLabelGroup = DeleteLabelGroup'
  { -- | The name of the label group that you want to delete. Data in this field
    -- will be retained for service usage. Follow best practices for the
    -- security of your data.
    labelGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLabelGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupName', 'deleteLabelGroup_labelGroupName' - The name of the label group that you want to delete. Data in this field
-- will be retained for service usage. Follow best practices for the
-- security of your data.
newDeleteLabelGroup ::
  -- | 'labelGroupName'
  Prelude.Text ->
  DeleteLabelGroup
newDeleteLabelGroup pLabelGroupName_ =
  DeleteLabelGroup'
    { labelGroupName =
        pLabelGroupName_
    }

-- | The name of the label group that you want to delete. Data in this field
-- will be retained for service usage. Follow best practices for the
-- security of your data.
deleteLabelGroup_labelGroupName :: Lens.Lens' DeleteLabelGroup Prelude.Text
deleteLabelGroup_labelGroupName = Lens.lens (\DeleteLabelGroup' {labelGroupName} -> labelGroupName) (\s@DeleteLabelGroup' {} a -> s {labelGroupName = a} :: DeleteLabelGroup)

instance Core.AWSRequest DeleteLabelGroup where
  type
    AWSResponse DeleteLabelGroup =
      DeleteLabelGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteLabelGroupResponse'

instance Prelude.Hashable DeleteLabelGroup where
  hashWithSalt _salt DeleteLabelGroup' {..} =
    _salt `Prelude.hashWithSalt` labelGroupName

instance Prelude.NFData DeleteLabelGroup where
  rnf DeleteLabelGroup' {..} =
    Prelude.rnf labelGroupName

instance Data.ToHeaders DeleteLabelGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.DeleteLabelGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteLabelGroup where
  toJSON DeleteLabelGroup' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("LabelGroupName" Data..= labelGroupName)
          ]
      )

instance Data.ToPath DeleteLabelGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteLabelGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteLabelGroupResponse' smart constructor.
data DeleteLabelGroupResponse = DeleteLabelGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteLabelGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteLabelGroupResponse ::
  DeleteLabelGroupResponse
newDeleteLabelGroupResponse =
  DeleteLabelGroupResponse'

instance Prelude.NFData DeleteLabelGroupResponse where
  rnf _ = ()
