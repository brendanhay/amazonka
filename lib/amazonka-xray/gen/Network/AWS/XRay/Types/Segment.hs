{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Segment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Segment where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A segment from a trace that has been ingested by the X-Ray service. The segment can be compiled from documents uploaded with 'PutTraceSegments' , or an @inferred@ segment for a downstream service, generated from a subsegment sent by the service that called it.
--
--
-- For the full segment document schema, see <https://docs.aws.amazon.com/xray/latest/devguide/xray-api-segmentdocuments.html AWS X-Ray Segment Documents> in the /AWS X-Ray Developer Guide/ .
--
--
-- /See:/ 'segment' smart constructor.
data Segment = Segment'
  { _sDocument :: !(Maybe Text),
    _sId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Segment' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sDocument' - The segment document.
--
-- * 'sId' - The segment's ID.
segment ::
  Segment
segment = Segment' {_sDocument = Nothing, _sId = Nothing}

-- | The segment document.
sDocument :: Lens' Segment (Maybe Text)
sDocument = lens _sDocument (\s a -> s {_sDocument = a})

-- | The segment's ID.
sId :: Lens' Segment (Maybe Text)
sId = lens _sId (\s a -> s {_sId = a})

instance FromJSON Segment where
  parseJSON =
    withObject
      "Segment"
      (\x -> Segment' <$> (x .:? "Document") <*> (x .:? "Id"))

instance Hashable Segment

instance NFData Segment
