{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.CSVMappingParameters
  ( CSVMappingParameters (..),

    -- * Smart constructor
    mkCSVMappingParameters,

    -- * Lenses
    cmpRecordRowDelimiter,
    cmpRecordColumnDelimiter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides additional mapping information when the record format uses delimiters, such as CSV. For example, the following sample records use CSV format, where the records use the /'\n'/ as the row delimiter and a comma (",") as the column delimiter:
--
-- @"name1", "address1"@
-- @"name2", "address2"@
--
-- /See:/ 'mkCSVMappingParameters' smart constructor.
data CSVMappingParameters = CSVMappingParameters'
  { -- | Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
    recordRowDelimiter :: Lude.Text,
    -- | Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
    recordColumnDelimiter :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CSVMappingParameters' with the minimum fields required to make a request.
--
-- * 'recordRowDelimiter' - Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
-- * 'recordColumnDelimiter' - Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
mkCSVMappingParameters ::
  -- | 'recordRowDelimiter'
  Lude.Text ->
  -- | 'recordColumnDelimiter'
  Lude.Text ->
  CSVMappingParameters
mkCSVMappingParameters pRecordRowDelimiter_ pRecordColumnDelimiter_ =
  CSVMappingParameters'
    { recordRowDelimiter = pRecordRowDelimiter_,
      recordColumnDelimiter = pRecordColumnDelimiter_
    }

-- | Row delimiter. For example, in a CSV format, /'\n'/ is the typical row delimiter.
--
-- /Note:/ Consider using 'recordRowDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpRecordRowDelimiter :: Lens.Lens' CSVMappingParameters Lude.Text
cmpRecordRowDelimiter = Lens.lens (recordRowDelimiter :: CSVMappingParameters -> Lude.Text) (\s a -> s {recordRowDelimiter = a} :: CSVMappingParameters)
{-# DEPRECATED cmpRecordRowDelimiter "Use generic-lens or generic-optics with 'recordRowDelimiter' instead." #-}

-- | Column delimiter. For example, in a CSV format, a comma (",") is the typical column delimiter.
--
-- /Note:/ Consider using 'recordColumnDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmpRecordColumnDelimiter :: Lens.Lens' CSVMappingParameters Lude.Text
cmpRecordColumnDelimiter = Lens.lens (recordColumnDelimiter :: CSVMappingParameters -> Lude.Text) (\s a -> s {recordColumnDelimiter = a} :: CSVMappingParameters)
{-# DEPRECATED cmpRecordColumnDelimiter "Use generic-lens or generic-optics with 'recordColumnDelimiter' instead." #-}

instance Lude.FromJSON CSVMappingParameters where
  parseJSON =
    Lude.withObject
      "CSVMappingParameters"
      ( \x ->
          CSVMappingParameters'
            Lude.<$> (x Lude..: "RecordRowDelimiter")
            Lude.<*> (x Lude..: "RecordColumnDelimiter")
      )

instance Lude.ToJSON CSVMappingParameters where
  toJSON CSVMappingParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RecordRowDelimiter" Lude..= recordRowDelimiter),
            Lude.Just ("RecordColumnDelimiter" Lude..= recordColumnDelimiter)
          ]
      )
