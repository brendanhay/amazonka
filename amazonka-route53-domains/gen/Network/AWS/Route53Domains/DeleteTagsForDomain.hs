{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.Route53Domains.DeleteTagsForDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations may
-- not immediately represent all issued operations.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DeleteTagsForDomain.html>
module Network.AWS.Route53Domains.DeleteTagsForDomain
    (
    -- * Request
      DeleteTagsForDomain
    -- ** Request constructor
    , deleteTagsForDomain
    -- ** Request lenses
    , dtfdDomainName
    , dtfdTagsToDelete

    -- * Response
    , DeleteTagsForDomainResponse
    -- ** Response constructor
    , deleteTagsForDomainResponse
    -- ** Response lenses
    , dtfdrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53Domains.Types

-- | The DeleteTagsForDomainRequest includes the following elements.
--
-- /See:/ 'deleteTagsForDomain' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtfdDomainName'
--
-- * 'dtfdTagsToDelete'
data DeleteTagsForDomain = DeleteTagsForDomain'
    { _dtfdDomainName   :: !Text
    , _dtfdTagsToDelete :: ![Text]
    } deriving (Eq,Read,Show)

-- | 'DeleteTagsForDomain' smart constructor.
deleteTagsForDomain :: Text -> DeleteTagsForDomain
deleteTagsForDomain pDomainName =
    DeleteTagsForDomain'
    { _dtfdDomainName = pDomainName
    , _dtfdTagsToDelete = mempty
    }

-- | The domain for which you want to delete one or more tags.
--
-- The name of a domain.
--
-- Type: String
--
-- Default: None
--
-- Constraints: The domain name can contain only the letters a through z,
-- the numbers 0 through 9, and hyphen (-). Hyphens are allowed only when
-- theyaposre surrounded by letters, numbers, or other hyphens. You
-- canapost specify a hyphen at the beginning or end of a label. To specify
-- an Internationalized Domain Name, you must convert the name to Punycode.
--
-- Required: Yes
dtfdDomainName :: Lens' DeleteTagsForDomain Text
dtfdDomainName = lens _dtfdDomainName (\ s a -> s{_dtfdDomainName = a});

-- | A list of tag keys to delete.
--
-- Type: A list that contains the keys of the tags that you want to delete.
--
-- Default: None
--
-- Required: No
--
-- \'>
dtfdTagsToDelete :: Lens' DeleteTagsForDomain [Text]
dtfdTagsToDelete = lens _dtfdTagsToDelete (\ s a -> s{_dtfdTagsToDelete = a});

instance AWSRequest DeleteTagsForDomain where
        type Sv DeleteTagsForDomain = Route53Domains
        type Rs DeleteTagsForDomain =
             DeleteTagsForDomainResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTagsForDomainResponse' <$> (pure (fromEnum s)))

instance ToHeaders DeleteTagsForDomain where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Route53Domains_v20140515.DeleteTagsForDomain" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTagsForDomain where
        toJSON DeleteTagsForDomain'{..}
          = object
              ["DomainName" .= _dtfdDomainName,
               "TagsToDelete" .= _dtfdTagsToDelete]

instance ToPath DeleteTagsForDomain where
        toPath = const "/"

instance ToQuery DeleteTagsForDomain where
        toQuery = const mempty

-- | /See:/ 'deleteTagsForDomainResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtfdrStatus'
newtype DeleteTagsForDomainResponse = DeleteTagsForDomainResponse'
    { _dtfdrStatus :: Int
    } deriving (Eq,Read,Show)

-- | 'DeleteTagsForDomainResponse' smart constructor.
deleteTagsForDomainResponse :: Int -> DeleteTagsForDomainResponse
deleteTagsForDomainResponse pStatus =
    DeleteTagsForDomainResponse'
    { _dtfdrStatus = pStatus
    }

-- | FIXME: Undocumented member.
dtfdrStatus :: Lens' DeleteTagsForDomainResponse Int
dtfdrStatus = lens _dtfdrStatus (\ s a -> s{_dtfdrStatus = a});
