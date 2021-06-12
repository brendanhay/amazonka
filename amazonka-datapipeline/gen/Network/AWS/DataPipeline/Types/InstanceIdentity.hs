{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.Types.InstanceIdentity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DataPipeline.Types.InstanceIdentity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identity information for the EC2 instance that is hosting the task
-- runner. You can get this value by calling a metadata URI from the EC2
-- instance. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata>
-- in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value
-- proves that your task runner is running on an EC2 instance, and ensures
-- the proper AWS Data Pipeline service charges are applied to your
-- pipeline.
--
-- /See:/ 'newInstanceIdentity' smart constructor.
data InstanceIdentity = InstanceIdentity'
  { -- | A description of an EC2 instance that is generated when the instance is
    -- launched and exposed to the instance via the instance metadata service
    -- in the form of a JSON representation of an object.
    document :: Core.Maybe Core.Text,
    -- | A signature which can be used to verify the accuracy and authenticity of
    -- the information provided in the instance identity document.
    signature :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceIdentity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'document', 'instanceIdentity_document' - A description of an EC2 instance that is generated when the instance is
-- launched and exposed to the instance via the instance metadata service
-- in the form of a JSON representation of an object.
--
-- 'signature', 'instanceIdentity_signature' - A signature which can be used to verify the accuracy and authenticity of
-- the information provided in the instance identity document.
newInstanceIdentity ::
  InstanceIdentity
newInstanceIdentity =
  InstanceIdentity'
    { document = Core.Nothing,
      signature = Core.Nothing
    }

-- | A description of an EC2 instance that is generated when the instance is
-- launched and exposed to the instance via the instance metadata service
-- in the form of a JSON representation of an object.
instanceIdentity_document :: Lens.Lens' InstanceIdentity (Core.Maybe Core.Text)
instanceIdentity_document = Lens.lens (\InstanceIdentity' {document} -> document) (\s@InstanceIdentity' {} a -> s {document = a} :: InstanceIdentity)

-- | A signature which can be used to verify the accuracy and authenticity of
-- the information provided in the instance identity document.
instanceIdentity_signature :: Lens.Lens' InstanceIdentity (Core.Maybe Core.Text)
instanceIdentity_signature = Lens.lens (\InstanceIdentity' {signature} -> signature) (\s@InstanceIdentity' {} a -> s {signature = a} :: InstanceIdentity)

instance Core.Hashable InstanceIdentity

instance Core.NFData InstanceIdentity

instance Core.ToJSON InstanceIdentity where
  toJSON InstanceIdentity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("document" Core..=) Core.<$> document,
            ("signature" Core..=) Core.<$> signature
          ]
      )
