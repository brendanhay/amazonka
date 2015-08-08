{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53Domains.DeleteTagsForDomain
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes the specified tags for a domain.
--
-- All tag operations are eventually consistent; subsequent operations may
-- not immediately represent all issued operations.
--
-- /See:/ <http://docs.aws.amazon.com/Route53/latest/APIReference/api-DeleteTagsForDomain.html AWS API Reference> for DeleteTagsForDomain.
module Network.AWS.Route53Domains.DeleteTagsForDomain
    (
    -- * Creating a Request
      DeleteTagsForDomain
    , deleteTagsForDomain
    -- * Request Lenses
    , dtfdDomainName
    , dtfdTagsToDelete

    -- * Destructuring the Response
    , DeleteTagsForDomainResponse
    , deleteTagsForDomainResponse
    -- * Response Lenses
    , dtfdrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTagsForDomain' smart constructor.
deleteTagsForDomain :: Text -> DeleteTagsForDomain
deleteTagsForDomain pDomainName_ =
    DeleteTagsForDomain'
    { _dtfdDomainName = pDomainName_
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
dtfdTagsToDelete = lens _dtfdTagsToDelete (\ s a -> s{_dtfdTagsToDelete = a}) . _Coerce;

instance AWSRequest DeleteTagsForDomain where
        type Sv DeleteTagsForDomain = Route53Domains
        type Rs DeleteTagsForDomain =
             DeleteTagsForDomainResponse
        request = postJSON
        response
          = receiveEmpty
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
-- * 'dtfdrsStatus'
newtype DeleteTagsForDomainResponse = DeleteTagsForDomainResponse'
    { _dtfdrsStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTagsForDomainResponse' smart constructor.
deleteTagsForDomainResponse :: Int -> DeleteTagsForDomainResponse
deleteTagsForDomainResponse pStatus_ =
    DeleteTagsForDomainResponse'
    { _dtfdrsStatus = pStatus_
    }

-- | Undocumented member.
dtfdrsStatus :: Lens' DeleteTagsForDomainResponse Int
dtfdrsStatus = lens _dtfdrsStatus (\ s a -> s{_dtfdrsStatus = a});
