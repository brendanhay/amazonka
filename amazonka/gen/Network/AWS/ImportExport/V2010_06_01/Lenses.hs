{-# LANGUAGE TemplateHaskell             #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.ImportExport.V2010_06_01.Lenses
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.ImportExport.V2010_06_01.Lenses where

import Control.Lens.TH
import Network.AWS.ImportExport.V2010_06_01.Types
import Network.AWS.ImportExport.V2010_06_01.CreateJob
import Network.AWS.ImportExport.V2010_06_01.ListJobs
import Network.AWS.ImportExport.V2010_06_01.UpdateJob
import Network.AWS.ImportExport.V2010_06_01.GetStatus
import Network.AWS.ImportExport.V2010_06_01.CancelJob

-- Newtypes

-- Products
makeLenses ''Job

-- Requests
makeLenses ''CreateJob
makeLenses ''ListJobs
makeLenses ''UpdateJob
makeLenses ''GetStatus
makeLenses ''CancelJob

-- Responses
makeLenses ''CreateJobResponse
makeLenses ''ListJobsResponse
makeLenses ''UpdateJobResponse
makeLenses ''GetStatusResponse
makeLenses ''CancelJobResponse
