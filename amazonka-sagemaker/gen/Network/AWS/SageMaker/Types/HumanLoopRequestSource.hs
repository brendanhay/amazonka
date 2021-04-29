{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.HumanLoopRequestSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopRequestSource where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.AwsManagedHumanLoopRequestSource

-- | Container for configuring the source of human task requests.
--
-- /See:/ 'newHumanLoopRequestSource' smart constructor.
data HumanLoopRequestSource = HumanLoopRequestSource'
  { -- | Specifies whether Amazon Rekognition or Amazon Textract are used as the
    -- integration source. The default field settings and JSON parsing rules
    -- are different based on the integration source. Valid values:
    awsManagedHumanLoopRequestSource :: AwsManagedHumanLoopRequestSource
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'HumanLoopRequestSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsManagedHumanLoopRequestSource', 'humanLoopRequestSource_awsManagedHumanLoopRequestSource' - Specifies whether Amazon Rekognition or Amazon Textract are used as the
-- integration source. The default field settings and JSON parsing rules
-- are different based on the integration source. Valid values:
newHumanLoopRequestSource ::
  -- | 'awsManagedHumanLoopRequestSource'
  AwsManagedHumanLoopRequestSource ->
  HumanLoopRequestSource
newHumanLoopRequestSource
  pAwsManagedHumanLoopRequestSource_ =
    HumanLoopRequestSource'
      { awsManagedHumanLoopRequestSource =
          pAwsManagedHumanLoopRequestSource_
      }

-- | Specifies whether Amazon Rekognition or Amazon Textract are used as the
-- integration source. The default field settings and JSON parsing rules
-- are different based on the integration source. Valid values:
humanLoopRequestSource_awsManagedHumanLoopRequestSource :: Lens.Lens' HumanLoopRequestSource AwsManagedHumanLoopRequestSource
humanLoopRequestSource_awsManagedHumanLoopRequestSource = Lens.lens (\HumanLoopRequestSource' {awsManagedHumanLoopRequestSource} -> awsManagedHumanLoopRequestSource) (\s@HumanLoopRequestSource' {} a -> s {awsManagedHumanLoopRequestSource = a} :: HumanLoopRequestSource)

instance Prelude.FromJSON HumanLoopRequestSource where
  parseJSON =
    Prelude.withObject
      "HumanLoopRequestSource"
      ( \x ->
          HumanLoopRequestSource'
            Prelude.<$> (x Prelude..: "AwsManagedHumanLoopRequestSource")
      )

instance Prelude.Hashable HumanLoopRequestSource

instance Prelude.NFData HumanLoopRequestSource

instance Prelude.ToJSON HumanLoopRequestSource where
  toJSON HumanLoopRequestSource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "AwsManagedHumanLoopRequestSource"
                  Prelude..= awsManagedHumanLoopRequestSource
              )
          ]
      )
