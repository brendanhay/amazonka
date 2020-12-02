{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.CurrentRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.CurrentRevision where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents information about a current revision.
--
--
--
-- /See:/ 'currentRevision' smart constructor.
data CurrentRevision = CurrentRevision'
  { _crRevisionSummary ::
      !(Maybe Text),
    _crCreated :: !(Maybe POSIX),
    _crRevision :: !Text,
    _crChangeIdentifier :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CurrentRevision' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crRevisionSummary' - The summary of the most recent revision of the artifact.
--
-- * 'crCreated' - The date and time when the most recent revision of the artifact was created, in timestamp format.
--
-- * 'crRevision' - The revision ID of the current version of an artifact.
--
-- * 'crChangeIdentifier' - The change identifier for the current revision.
currentRevision ::
  -- | 'crRevision'
  Text ->
  -- | 'crChangeIdentifier'
  Text ->
  CurrentRevision
currentRevision pRevision_ pChangeIdentifier_ =
  CurrentRevision'
    { _crRevisionSummary = Nothing,
      _crCreated = Nothing,
      _crRevision = pRevision_,
      _crChangeIdentifier = pChangeIdentifier_
    }

-- | The summary of the most recent revision of the artifact.
crRevisionSummary :: Lens' CurrentRevision (Maybe Text)
crRevisionSummary = lens _crRevisionSummary (\s a -> s {_crRevisionSummary = a})

-- | The date and time when the most recent revision of the artifact was created, in timestamp format.
crCreated :: Lens' CurrentRevision (Maybe UTCTime)
crCreated = lens _crCreated (\s a -> s {_crCreated = a}) . mapping _Time

-- | The revision ID of the current version of an artifact.
crRevision :: Lens' CurrentRevision Text
crRevision = lens _crRevision (\s a -> s {_crRevision = a})

-- | The change identifier for the current revision.
crChangeIdentifier :: Lens' CurrentRevision Text
crChangeIdentifier = lens _crChangeIdentifier (\s a -> s {_crChangeIdentifier = a})

instance Hashable CurrentRevision

instance NFData CurrentRevision

instance ToJSON CurrentRevision where
  toJSON CurrentRevision' {..} =
    object
      ( catMaybes
          [ ("revisionSummary" .=) <$> _crRevisionSummary,
            ("created" .=) <$> _crCreated,
            Just ("revision" .= _crRevision),
            Just ("changeIdentifier" .= _crChangeIdentifier)
          ]
      )
