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
-- Module      : Network.AWS.S3.Types.JSONOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.JSONOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Specifies JSON as request\'s output serialization format.
--
-- /See:/ 'newJSONOutput' smart constructor.
data JSONOutput = JSONOutput'
  { -- | The value used to separate individual records in the output. If no value
    -- is specified, Amazon S3 uses a newline character (\'\\n\').
    recordDelimiter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'JSONOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordDelimiter', 'jSONOutput_recordDelimiter' - The value used to separate individual records in the output. If no value
-- is specified, Amazon S3 uses a newline character (\'\\n\').
newJSONOutput ::
  JSONOutput
newJSONOutput =
  JSONOutput' {recordDelimiter = Prelude.Nothing}

-- | The value used to separate individual records in the output. If no value
-- is specified, Amazon S3 uses a newline character (\'\\n\').
jSONOutput_recordDelimiter :: Lens.Lens' JSONOutput (Prelude.Maybe Prelude.Text)
jSONOutput_recordDelimiter = Lens.lens (\JSONOutput' {recordDelimiter} -> recordDelimiter) (\s@JSONOutput' {} a -> s {recordDelimiter = a} :: JSONOutput)

instance Prelude.Hashable JSONOutput

instance Prelude.NFData JSONOutput

instance Prelude.ToXML JSONOutput where
  toXML JSONOutput' {..} =
    Prelude.mconcat
      ["RecordDelimiter" Prelude.@= recordDelimiter]
